;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Simple utils to define unit tests.
;;;
;;; signalsp - returns a signaled condition of specified type or nil.
;;; assert-error - asserts that a form will signal an error.
;;; macro-error - returns an error cased by macroexpanding a form or nil.
;;; assert-macro-error - asserts that a form signals an error at macroexpansion time.
;;; deftest - has a defun like signal and registers the function as unit test.
;;;

(defpackage #:ace.test
  (:use #:cl #:ace.core #:ace.core.macro)
  (:import-from #:ace.test.runner
                #:*unit-tests*
                #:assign-test-fixture-functions
                #:fixture
                #:run-tests)
  #+bordeaux-threads
  (:import-from #:bordeaux-threads #:make-recursive-lock #:with-recursive-lock-held)
  (:export
   ;; Testing utilities.
   #:signals
   #:signalsp
   #:check
   #:expect
   #:assert-error
   #:assert-macro-error
   #:expect-error
   #:expect-macro-error
   #:expect-warning
   #:expect-macro-warning
   #:define-test-fixture
   #:deftest
   #:letf*
   ;; Mocking
   #:with-mock-foreign-functions
   #:with-mock-functions
   #:with-mock-functions*
   ;; Execution
   #:run-tests))

(in-package #:ace.test)

#+(and sbcl (not bordeaux-threads))
(progn
  (defun make-recursive-lock (name) (sb-thread:make-mutex :name name))
  (defmacro with-recursive-lock-held ((lock) &body body)
    `(sb-thread:with-recursive-lock (,lock) ,@body)))

;;; Test utilities.

(defvar *fixture-counters*
  (make-hash-table :test #'equal)
  "Map of file name to current fixture identifier (small integer)")
(defun current-fixture-id ()
  (let* ((file (current-file-namestring))
         (n (gethash file *fixture-counters*)))
    (and n (format nil "~@[~A~]_~D" file n))))

(defun add-test (fixture name)
  "Adds a test with the `NAME' to the list of unit-tests.

Parameters:
 `FIXTURE' is the fixture ID associated with the test.
 `NAME' the symbol-name of the test.
"
  (declare (symbol name))
  (pushnew name *unit-tests*)
  (setf (get name 'fixture) fixture))

(defmacro define-test-fixture (&key setup teardown)
  "Defines SETUP and TEARDOWN hooks for subsequent unit tests in the current file.
SETUP is evaluated once before the first scheduled test in the fixture group.
TEARDOWN is evaluated once after the last scheduled test in the fixture group."
  (let* ((id (let* ((file (current-file-namestring))
                    (n (incf (gethash file *fixture-counters* 0))))
               (format nil "~@[~A~]_~D" file n)))
         (prelude-fn (intern (format nil "~A-PRELUDE" id) *package*))
         (teardown-fn (intern (format nil "~A-TEARDOWN" id) *package*)))
    `(progn
       (defun ,prelude-fn () ,setup)
       (defun ,teardown-fn () ,teardown)
       (assign-test-fixture-functions ,id
                                      ,(and setup `#',prelude-fn)
                                      ,(and teardown `#',teardown-fn)))))

(defmacro deftest (name &rest args-and-body)
  "Defines a test named `NAME' as a function. Registers it with other tests.

Parameters:
 `ARGS-AND-BODY' - [:order t] (ARGS*) BODY.
 `ARGS' is a lambda list with only optional, keyword, or rest arguments.

  A deftest fails if an error is signalled from within."
  (check-type name symbol)
  (when (eq (car args-and-body) :order)
    (pop args-and-body)
    (let ((order (pop args-and-body)))
      (check-type order (eql t))))
  (let ((args (pop args-and-body))
        (body args-and-body))
    (check-type args (or null (cons (member &optional &key &rest))))
    `(progn
       (add-test ,(current-fixture-id) ',name)
       (defun ,name ,args . ,body))))

(defvar *global-junk* nil "Avoid flushing results in SIGNALS.")

(defmacro signals (&environment env condition &body body)
  "Returns the expected CONDITION or NIL.

Example:
 (assert (signals warning (warn \"This warning should be detected\")))
"
  (check-type condition symbol)
  (unless (subtypep condition 'condition env)
    (error "~S does not designate any condition type." condition))
  `(handler-case (progn ,@body)
     (,condition (e) e)
     (:no-error (&rest results)
       (let ((len (length results)))
         (setf *global-junk* len)
         nil))))

;; TODO(czak): Remove.
(defmacro signalsp (condition &body body)
  "True if the BODY signals a subtype of CONDITION.

 Example:
  (assert (signalsp warning (warn \"This warning should be detected\")))"
  `(signals ,condition ,@body))

(defmacro assert-error (&body body)
  "Asserts that execution of the BODY causes an error."
  `(check (signals error ,@body)))

(defmacro expect-error (&body body)
  "Expects that execution of the BODY causes an error."
  `(expect (signals error ,@body)))

(defmacro expect-warning (&body body)
  "Expects that execution of the BODY causes an error."
  `(expect (signals warning ,@body)))

(defmacro assert-macro-error (body)
  "Asserts that macroexpansion of the BODY results in an ERROR."
  `(assert-error (macroexpand* ',body)))

(defmacro expect-macro-error (body)
  "Expects that macroexpansion of the BODY results in an ERROR."
  `(expect-error (macroexpand* ',body)))

(defmacro expect-macro-warning (body)
  "Expects that macroexpansion of the BODY results in an ERROR."
  `(expect-warning (macroexpand* ',body)))

;;;
;;; Convenience for testing bad/unsafe legacy code that depends on global state.

(defvar *unsafe-code-test-mutex* (make-recursive-lock "UNSAFE-CODE-TEST-MUTEX")
  "Used to serialize tests that mutate global space.")

(defun %with-letf*-bindings (fn revert values)
  (declare (function fn revert) (list values))
  (with-recursive-lock-held (*unsafe-code-test-mutex*)
    (unwind-protect (funcall fn)
      (apply revert values))))

(defmacro letf* (clauses &body body)
  "Sets the places specified in CLAUSES as (place value [old-value])
to the values for the dynamic scope of LETF* invocation.
This is reversed thereafter - using the value of PLACE or the OLD-VALUE.
Note that LETF* has nothing to do with LET* besides syntax.
E.g. it will not create a new binding as it requires a settable place.
The execution of LETF* is serialized through *UNSAFE-CODE-TEST-MUTEX*.

WARNING: Use LETF* as a last resort when there is no way to change
the code and to provide test hooks or proper test interfaces."
  (let* ((places (mapcar #'first clauses))
         (gensyms (mapcar #'gensym* places)))
    `(%with-letf*-bindings
      (lambda ()
        (setf ,@(lconc ((p v) clauses) `(,p ,v)))
        (locally ,@body))
      (lambda ,gensyms
        (setf ,@(mapcan #'list places gensyms)))
      `(,,@(lmap ((p v ov) clauses) (or ov p))))))

;;; Mocks

;;; Note: you should seldom if ever use the form of this macro which rebinds global names
;;; to arbitrary local names. If needed, it's equivalent to writing your own LET bindings:
;;;  (LET ((original-fn-1 #'fn1) (original-fn2 #'fn2)) (WITH-MOCKED-FUNCTIONS ...))
;;; which have the further benefit of being unmistakable for a call to the wrong name.
(defmacro with-mock-functions (bindings &body body &environment env)
  "Executes the BODY with the functions mocked in BINDINGS.
Each BINDING is a
  (function-name (lambda (...) ...) [real]) or
  (function-name #'mock [real]).

If a REAL symbol is provided with the binding, it is bound to the real function
within the mock-bindings and within body. This allows the mock functions to
call into the real functions.

WITH-MOCK-FUNCTIONS is protected by a recursive mutex and runs serially
wrt. other WITH-MOCK-FUNCTIONS.

Note that WITH-MOCK-FUNCTIONS overrides the function definition temporarily.
In SBCL the override may not be propagated to all threads in a timely manner.
I.e. access to a function definition is not atomic or synchronized
and your tests will be flaky if you expect that other running threads will
pick up the changes in a timely manner magically.

Use WITH-MOCK-FUNCTIONS as a last resort when there is no way to change
the code and to provide test hooks or proper test interfaces."
  (loop :for (function) :in bindings :do
    (expect (not (inline-function-p function))
            "Overriding an inline function ~S will not work." function)
    (expect (not (compiler-macro-function function env))
            "Overriding a function with a compiler-macro ~S will not work."
            function)
    (expect (not (function-has-transforms-p function))
            "Overriding a function with a source transforms ~S will not work."
            function))
  #+sbcl
  (loop for (name definition local) in bindings
        collect name into names
        collect `(lambda (.f. &rest .arglist.)
                   (flet ((,(or local (setq local (make-symbol "_"))) (&rest r)
                            (apply .f. r)))
                     (declare (ignorable #',local))
                     (apply (the function ,definition) .arglist.)))
        into mocks
        finally (return `(call-with-mocks (lambda () ,@body) ',names ,@mocks)))
  #-sbcl
  (let ((fvars (lmap ((f) bindings) `(,(gensym* f) #',f))))
    `(with-recursive-lock-held (*unsafe-code-test-mutex*)
       (let ,fvars ;; Save the functions under gensym vars.
         (declare (function ,@(mapcar #'car fvars)))
         ;; Declare the real functions with the specified name (R)
         (flet ,(lconc ((g) fvars) ((f v r) bindings)
                       (and r `((,r (&rest args) (apply ,g args)))))
           ;; Use LETF* to override the (FDEFINITION ...) place
           ;; with new value (V)
           ;; and revert it using the old value (G) later.
           (letf* ,(lmap ((f v) bindings)
                         ((g)   fvars)
                         `((fdefinition ',f) ,v ,g))
             ,@body))))))

(defmacro with-mock-functions* (bindings &body body)
  "Executes the BODY with the functions mocked in BINDINGS.
Each BINDING is a
  (function-name (args) mock-body).

WITH-MOCK-FUNCTIONS* is protected by a recursive mutex and runs serially
wrt. other WITH-MOCK-FUNCTIONS* or WITH-MOCK-FUNCTIONS.

The WITH-MOCK-FUNCTIONS* is similar to WITH-MOCK-FUNCTIONS except it
does not allow to specify the mock using a lambda or #'mock form.

Use WITH-MOCK-FUNCTIONS* as a last resort when there is no way to change
the code and to provide test hooks or proper test interfaces."
  #+sbcl
  (loop for (name lambdavars . forms) in bindings
        collect name into names
        collect `(lambda (_ ,@lambdavars) (declare (ignore _)) ,@forms) into wrappers
        finally (return `(call-with-mocks (lambda () ,@body) ',names ,@wrappers)))
  #-sbcl
  `(with-mock-functions
       ,(loop :for b :in bindings
              :collect `(,(first b) (lambda ,@(rest b))))
     ,@body))

#+sbcl
(defun call-with-mocks (thunk names &rest wrappers)
  (sb-int:aver (= (length wrappers) (length names)))
  (with-recursive-lock-held (*unsafe-code-test-mutex*)
    (unwind-protect
         (progn
           (mapc (lambda (name wrapper)
                   ;; Prevent mocked mocks. Too confusing who sees what
                   (sb-int:aver (not (sb-int:encapsulated-p name 'mock)))
                   (sb-int:encapsulate name 'mock wrapper))
                 names wrappers)
           (funcall thunk))
      (dolist (name names)
        (sb-int:unencapsulate name 'mock)))))

#+sbcl
(progn
(defmacro with-mock-foreign-functions (name-mapping &body body)
  `(let ((routines-to-restore (intercept-alien-linkage ',name-mapping)))
     (unwind-protect (progn ,@body)
       (restore-alien-linkage routines-to-restore))))

(defun intercept-alien-linkage (name-mapping &aux restore)
  ;; name mapping is a list of pairs: (c-name replacement)
  ;; where c-name is a string and replacement is an alien-callable.
  (dolist (pair name-mapping restore)
    (push (sb-alien-internals:override-alien-linkage-entrypoint (car pair) (cadr pair)) restore)))

(defun restore-alien-linkage (pairs)
  (dolist (pair pairs) (setf (sb-sys:sap-ref-word (car pair) 0) (cdr pair)))))
