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
                #:run-tests
                #:order
                #:timeout)
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
   #:deftest
   #:letf*
   ;; Mocking
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

(defun add-test (name &key order timeout)
  "Adds a test with the `NAME' to the list of unit-tests.

Parameters:
 `NAME' the symbol-name of the test.
 `ORDER' is the order parameter on the test used to execute tests in order.
 `TIMEOUT' is the timeout for the test in seconds.
"
  (declare (symbol name))
  (pushnew name *unit-tests*)
  (when order (setf (get name 'order) order))
  (when timeout (setf (get name 'timeout) timeout)))

(defun parse-deftest-options (options-args-body)
  "Returns (values order timeout args body) parsed out of OPTIONS-ARGS-BODY."
  (let ((order t) timeout args body)
    (loop :while
          (case (car options-args-body)
            (:order
             (pop options-args-body)
             (setf order (pop options-args-body))
             t)
            (:timeout
             (pop options-args-body)
             (setf timeout (pop options-args-body))
             t)))
    (setf args (pop options-args-body)
          body options-args-body)
    (check-type args (or null (cons (member &optional &key &rest))))
    (check-type order (or boolean number))
    (check-type timeout (or null number))
    (values order timeout args body)))

(defmacro deftest (name &rest options-args-body)
  "Defines a test named `NAME' as a function. Registers it with other tests.

Parameters:
 `OPTIONS-ARGS-BODY' - [:order ORDER|:timeout TIMEOUT]* (ARGS*) BODY.
 `ARGS' is a lambda list with only optional, keyword, or rest arguments.
 `ORDER' indicates controls the order of tests and whether the test can
  run at the same time as other tests.

  Tests are run in the following order:
   `ORDER' negative: Run one at a time, from most-negative to least-negative.
   `ORDER' NIL: run in parallel with any other ORDER: NIL tests.
   `ORDER' positive: Run one at a time from least-positive to most-positive.
   `ORDER' T: Run one at a time.

  TIMEOUT' specifies the maxim time given to a test in seconds.

  A deftest fails if an error is signalled from within."
  (check-type name symbol)
  (multiple-value-bind (order timeout args body)
      (parse-deftest-options options-args-body)
    `(progn
       (add-test ',name
                 ,@(when order `(:order ,order))
                 ,@(when timeout `(:timeout ,timeout)))
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

;;; Mocks - TODO(czak): Move to mocks.lisp.

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
  (loop for (name wrapper localname) in bindings
        collect name into names
        collect `(lambda (.f. &rest .args.)
                   ,@(if localname ; user wants to call the underlying function
                         `((flet ((,localname (&rest _) (apply (the function .f.) _)))
                             (apply ,wrapper .args.)))
                         `((declare (ignore .f.)) ; user doesn't want ...
                           (apply ,wrapper .args.))))
        into wrappers
        finally (return `(call-with-mocks (lambda () ,@body) ',names ,@wrappers)))
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
                   ;; This would have to gensym an identifier for the encapsulation
                   ;; if we want to support mocking a mocked function.
                   (sb-int:aver (not (sb-int:encapsulated-p name 'mock)))
                   (sb-int:encapsulate name 'mock wrapper))
                 names wrappers)
           (funcall thunk))
      (dolist (name names)
        (sb-int:unencapsulate name 'mock)))))
