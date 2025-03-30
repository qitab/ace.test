;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; The test runner runs //lisp/test ace.test.deftest tests.
;;;
;;; run-tests - will run all the tests and return whether tests pass.
;;; deregister-tests - removes unit tests from the active list based on a selector.
;;;
;;; This package contains an implementation of unit test runner.
;;; RUN-TESTS is the function invoked to run tests.
;;;

(defpackage #:ace.test.runner
  (:use #:common-lisp #:ace.core)
  (:import-from #:ace.core.tty
                #:ttyp
                #:*print-ansi*)
  (:import-from #:ace.core.os
                #:getenv)
  (:import-from #:ace.core.macro
                #:eval-always)
  (:import-from #:ace.core.check.condition
                #:failed
                #:missed
                #:*on-missed-expectation*
                #:alternate-truth-form)
  (:export
   ;; Execution of tests.
   #:*checks-count*
   #:*failed-conditions*
   #:failed-conditions
   #:make-schedule
   #:nothing-tested
   #:run-tests
   #:*reporting-hooks*
   #:run-and-report-tests
   #:deregister-tests
   #:*debug-unit-tests*
   ;; TIMEOUT is a symbol naming a condition, and it was confusing to also have it name a function,
   ;; so the function is now named DEFAULT-TIMEOUT
   #:default-timeout
   #:order))

(in-package #:ace.test.runner)

;;; Compatibility shims
#+bordeaux-threads
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (import '(bordeaux-threads:make-thread bordeaux-threads:join-thread
              bordeaux-threads:with-timeout bordeaux-threads:timeout)))
  (defun make-mutex (name) (bordeaux-threads:make-lock name))
  (defmacro with-mutex ((lock) &body body) `(bordeaux-threads:with-lock-held (,lock) ,@body)))
#+(and sbcl (not bordeaux-threads))
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (import '(sb-thread:make-thread sb-thread:join-thread sb-thread:with-mutex sb-ext:timeout)))
  (defmacro with-timeout ((time) &body body) `(sb-ext:with-timeout ,time ,@body))
  (defun make-mutex (name) (sb-thread:make-mutex :name name)))

;;; Test execution.

(declaim (list *unit-tests*))
(defvar *unit-tests* nil
  "A list of symbols representing the unit-tests executed with run-tests or check-tests.")

(declaim (type (unsigned-byte 62) *checks-count*))
(defvar *checks-count* 0
  "The number of conditions tested with CHECK/EXPECT. Bound by RUN-TEST to 0.")

(declaim (list *failed-conditions*))
(defvar *failed-conditions* nil
  "The list of failed condition objects. Bound by RUN-TEST to NIL.")

(defvar *failed-conditions-mutex* (make-mutex "failed-conditions-mutex.")
  "Protects the *failed-conditions*.")

;; Hook up to CHECK and EXPECT to get the proper counts.

(defmethod check :before (result &optional datum &rest arguments)
  (declare (ignore datum arguments))
  (incf *checks-count*))

(defmethod expect :before (result &optional datum &rest arguments)
  (declare (ignore datum arguments))
  (incf *checks-count*))

(defun register-failure (failure)
  "Registers a FAILURE condition adding it to *FAILED-CONDITIONS* variable."
  (with-mutex (*failed-conditions-mutex*)
    (push failure *failed-conditions*))
  (values))

(setf *on-missed-expectation* #'register-failure)

(defmethod alternate-truth-form :around (form)
  `(progn
     (incf *checks-count*)
     ,(call-next-method)))

;;; Parameters.

(declaim (boolean *parallel*))
(defvar *parallel* t "A flag for running tests in multiple threads.")

(declaim (boolean *threaded*))
(defvar *threaded* t "If true test are run in designated threads.")

(declaim (boolean *debug-unit-tests*))
(defvar *debug-unit-tests* nil "If non-nil, each error will invoke the debugger.")

(declaim (fixnum *status-column*))
(defvar *status-column* 80 "The column on which to print the test result.")

(defstruct test-run
  "The state and other properties of a test-run."
  ;; The name of the test.
  (test nil :type symbol)
  ;; Indicates if the test is run in a separate thread.
  (parallel nil :type boolean)
  ;; The timeout in seconds for the test.
  (timeout nil :type (or null number))
  ;; The test failure condition.
  (error nil :type (or null condition))
  ;; The backtrace for the error.
  (trace nil :type (or null string))
  ;; Used as the standard- and error-output.
  (output-stream nil :type (or null stream))
  (output-text "" :type string)
  ;; All the checks and expected conditions in tests.
  (checks-count 0 :type integer)
  ;; Failed non fatal conditions.
  (failed-conditions nil :type list)
  ;; Execution timing.
  (real-time-start 0 :type integer)
  (real-time-stop -1 :type integer)
  (run-time-start 0 :type integer)
  (run-time-stop -1 :type integer))

(defun test-run-real-time (run)
  "Returns the elapsed test RUN real time for the test in seconds or NIL."
  (let ((real-time-ticks
         (- (test-run-real-time-stop run) (test-run-real-time-start run))))
    (float (/ real-time-ticks internal-time-units-per-second))))

(defun test-run-run-time (run)
  "Returns the elapsed test RUN run time for the test in seconds or NIL."
  (let ((run-time-ticks
         (- (test-run-run-time-stop run) (test-run-run-time-start run))))
    (float (/ run-time-ticks internal-time-units-per-second))))

(defun test-run-start-time (run)
  "Set the start times on the test RUN."
  (setf (test-run-run-time-start run) (get-internal-run-time)
        (test-run-real-time-start run) (get-internal-real-time)))

(defun test-run-stop-time (run)
  "Set the stop times on the test RUN."
  (setf (test-run-run-time-stop run) (get-internal-run-time)
        (test-run-real-time-stop run) (get-internal-real-time)))

(defmethod print-object ((run test-run) stream)
  "Prints a test RUN object to the STREAM."
  (with-slots (test error checks-count failed-conditions) run
    (let ((failed-count (length failed-conditions)))
      (print-unreadable-object (run stream)
        (format stream
                "~S: ~:[OK~;~:*~S~]~:[~3*~; [~:[~*~;~D/~]~D]~]"
                test (and error (type-of error))
                (plusp checks-count) (plusp failed-count)
                failed-count checks-count)))))

(define-condition failed-conditions (failed) ()
  (:documentation "A type of FAILED that is returned if some expectations failed."))

(define-condition nothing-tested (failed) ()
  (:documentation "A type of FAILED that is returned if no check have been performed."))

;; Inlined so not to add clutter to the backtrace.
(declaim (inline update-test-run))
(defun update-test-run (run &optional error-condition)
  "Updates the test RUN. When ERROR-CONDITION is provided, UPDATE-TEST-RUN
adds the ERROR and a stack trace to the test RUN."
  (with-accessors ((test              test-run-test)
                   (error             test-run-error)
                   (trace             test-run-trace)
                   (checks-count      test-run-checks-count)
                   (failed-conditions test-run-failed-conditions)
                   (output-stream     test-run-output-stream)
                   (output-text       test-run-output-text)) run
    (setf checks-count *checks-count*
          failed-conditions (reverse *failed-conditions*))
    (test-run-stop-time run)
    (cond (error-condition
           (setf error error-condition
                 trace (or (ignore-errors
                            (with-output-to-string (out)
                              (trivial-backtrace:print-backtrace-to-stream out)))
                           "Error printing backtrace.")))
          (failed-conditions
           (setf error
                 (make-condition
                  'failed-conditions
                  :format-control
                  "The test ~S ended with ~D (out of ~D) failed conditions."
                  :format-arguments
                  (list test (length failed-conditions) checks-count))))
          ((zerop checks-count)
           (setf error
                 (make-condition
                  'nothing-tested
                  :format-control
                  "The test ~S ended with no conditions being tested. Use EXPECT or CHECK."
                  :format-arguments (list test)))))
    (when output-stream
      (setf output-text
            (concatenate 'string output-text (get-output-stream-string output-stream)))))
  run)

(defun separator-line (out &optional length)
  "Prints a line of '~' in LENGTH to the OUT stream."
  (format out "~&~v~~%" (or length (+ 20 *status-column*))))

(defun report-failure (run &key (out *error-output*))
  "Reports a failed unit-test.
 Arguments:
  RUN - is the test RUN object containing data to report.
  OUT - the stream to output the error information."
  (declare (optimize (speed 1) (safety 3)))
  (with-accessors ((test test-run-test)
                   (error test-run-error)
                   (trace test-run-trace)
                   (output-text test-run-output-text)) run
    (let* ((*package* (or (symbol-package test) *package*))
           (emsg (and error (ignore-errors (format nil "~@<~3I   ~A~:@>" error))))
           (*package* (find-package "COMMON-LISP-USER"))
           (*print-ansi* (ttyp out))
           ;; Correct the column count for ANSI escape characters.
           (offset (if *print-ansi* 9 0))
           (msg (with-output-to-string (str)
                  (format str "(~33@/ansi/)~vT=> ~31@/ansi/~%"
                          test (+ *status-column* offset) (type-of error))
                  (when (plusp (length output-text))
                    (format str "~%~A~&" output-text))
                  (when emsg
                    (format str "~%~A~&" (string-trim '(#\Newline) emsg)))
                  (format str "~@[~%~A~&~]" trace)
                  (separator-line str))))
      (finish-output out)
      (fresh-line out)
      (write-string msg out)
      (finish-output out))))

(defun evaluate-test-run (run &key verbose output)
  "Evaluates the test RUN. Prints info to the OUTPUT stream when VERBOSE.
Returns true if there was no error."
  (declare (stream output))
  (with-accessors ((error             test-run-error)
                   (output-text       test-run-output-text)
                   (checks-count      test-run-checks-count)
                   (failed-conditions test-run-failed-conditions)
                   (time              test-run-real-time)) run
    (when verbose
      (format output "~&(~(~A~))~vT=> ~v/ansi/"
              (test-run-test run) *status-column*
              (if error 31 32) (if error (type-of error) :PASSED))
      (cond (failed-conditions
             (format output " ~31/ansi/~@[ ~,3Fs~]~%"
                     (format nil "[~D/~D]" (length failed-conditions) checks-count)
                     (and (>= time 0.001) time)))
            ((plusp checks-count)
             (format output " [~D]~@[ ~,3Fs~]~%"
                     checks-count (and (>= time 0.001) time)))
            (t
             (terpri output)))
      (when (plusp (length output-text))
        (format output "~%~A~&" output-text)
        (separator-line output)))
    (not error)))

(defun %invoke-debugger (condition &key (package *package*))
  "Invokes the debugger for CONDITION. Assures that the debugger is invoked in the PACKAGE."
  ;; This makes sure that the user interaction is not obscured by long package names.
  ;; E.g SWANK would bind the package to *BUFFER-PACKAGE* which is the current at the REPL.
  (let* ((*package* package)
         (swank (find-package "SWANK"))
         (buffer-package (and swank (find-symbol "*BUFFER-PACKAGE*" swank))))
    (if buffer-package
        (progv (list buffer-package) (list package)
          (invoke-debugger condition))
        (invoke-debugger condition))))

;; TODO(czak): Move to a utility package.
(defmacro with-sane-io-syntax (&body body)
  "Sets an IO environment for the BODY with standard IO syntax plus error suppression."
  `(with-standard-io-syntax
     (let ((*print-readably* nil)
           #+sbcl (sb-ext:*suppress-print-errors* t))
       ,@body)))

(defun run-test (test &key (run (make-test-run :test test)) (debug *debug-unit-tests*))
  "Runs a single TEST capturing the output and errors into a test RUN object.
 DEBUG will bring up the debugger in an interactive setting when the test fails.
 Returns the test RUN object for the test."
  (with-sane-io-syntax
    (let* ((*debug-unit-tests* debug)
           (out (test-run-output-stream run))
           (*standard-output*
            (if (and debug out)
                (make-broadcast-stream out *standard-output*)
                (or out *standard-output*)))
           (*error-output*
            (if (and debug out)
                (make-broadcast-stream out *error-output*)
                (or out *error-output*)))
           (*debug-io*
            (make-two-way-stream
             *debug-io*
             (if (and debug out)
                 (make-broadcast-stream out *debug-io*)
                 (or out *debug-io*))))
           (*checks-count* 0)
           (*failed-conditions* nil)
           (*package* (or (symbol-package test) *package*)))
      (flet ((on-warning (warning)
               (when *debug-unit-tests*
                 (%invoke-debugger warning)))
             (on-error (error)
               (if *debug-unit-tests*
                   (%invoke-debugger error)
                   (return-from run-test (update-test-run run error)))))
        (test-run-start-time run)
        (handler-bind ((missed #'on-warning)
                       (error #'on-error)
                       (timeout #'on-error))
          (loop do
            (with-simple-restart (retry "Retry ~S" test)
              (return
                (if (test-run-timeout run)
                    (with-timeout ((test-run-timeout run))
                      (funcall (symbol-function test)))
                    (funcall (symbol-function test)))))))
        (update-test-run run)))))

(defun default-timeout ()
  "Return a value for the default test timeout in seconds."
  ;; The test timeout is provided by blaze test in the TEST_TIMEOUT variable.
  ;; See: http://bazel.build/docs/test-encyclopedia.html
  (let ((timeout (parse-integer (getenv "TEST_TIMEOUT" "0") :junk-allowed t)))
    (and (plusp timeout) timeout)))

(defun test-only-filter ()
  "Return a set of strings that will match names of the unit tests under test."
  (let ((test-only (getenv "TESTBRIDGE_TEST_ONLY")))
    (and test-only (ace.core.string:split test-only :by #\,))))

(defun fqn (symbol)
  "Return the name of the SYMBOL prefixed with the package name."
  (declare (symbol symbol))
  (let ((package (symbol-package symbol))
        (short (symbol-name symbol)))
    (if package
        (format nil "~A:~A" (package-name package) short)
        short)))

(defun filter-unit-tests (unit-tests filtered-names)
  "Return a list of UNIT-TESTS filtered down to the ones in FILTER-NAMES.
If FILTERED-NAMES in NIL, return the full list of UNIT-TESTS."
  (declare (list unit-tests filtered-names))
  (if filtered-names
      ;; Assume filtered-names is short.
      (loop :for ut :in unit-tests
            :when (or (member ut filtered-names :test #'string-equal)
                      (member (fqn ut) filtered-names :test #'string-equal))
              :collect ut)
      unit-tests))

(defgeneric make-schedule (tests &key parallel &allow-other-keys)
  (:documentation
   "Divide the TESTS in 3 groups:
 - PROLOGUE - serial tests run at start,
 - PARALLEL - run parallel in multiple threads,
 - EPILOGUE - serial tests run at the end.

Returns 2 lists as (VALUES PROLOGUE PARALLEL EPILOGUE).
If PARALLEL is NIL, the PARALLEL tests will be empty."))

(defun sort-tests (tests)
  "Sorts a list of TESTS in execution order and returns a copy."
  (stable-sort tests
               (lambda (x y)
                 (cond ((eql x t) nil)
                       ((eql y t) t)
                       ((< (or x 0) (or y 0)))))
               :key (lambda (test) (get test 'order))))

(defmethod make-schedule (tests &key (parallel *parallel*) &allow-other-keys)
  (let ((tests (sort-tests (reverse tests))))
    (unless parallel
      (return-from make-schedule
        (values tests nil nil)))

    (let (prologue parallel epilogue serial-tests-at-end)
      (dolist (test tests)
        (cond ((not (get test 'order))
               (setf serial-tests-at-end t)
               (push test parallel))
              (serial-tests-at-end
               (push test epilogue))
              (t
               (push test prologue))))

      (values (nreverse prologue)
              (nreverse parallel)
              (nreverse epilogue)))))

(defun %run-tests (&key
                   (debug *debug-unit-tests*)
                   (threaded (unless debug *threaded*))
                   (parallel (and threaded *parallel*))
                   (out *error-output*)
                   (verbose debug))
  "Runs all tests.
 Arguments:
  DEBUG    - when non-nil, send each test failure to the debugger
  PARALLEL - when non-nil, the tests may be executed in parallel on multiple threads
  THREADED - when nil, tests are run in the current thread.
  OUT      - stream to send output to
  VERBOSE  - when non-nil, output test test-run lines for each test
 Returns a list of all tests outcomes (as test-run objects) and a list of failed test outcomes."
  (let ((*debug-unit-tests* debug)
        (filtered-unit-tests
         (filter-unit-tests *unit-tests* (test-only-filter)))
        all-runs failed-runs
        prologue parallel-tests epilogue)
    (declare (list failed-runs all-runs))
    (multiple-value-setq (prologue parallel-tests epilogue)
      (make-schedule filtered-unit-tests :parallel parallel))
    (when verbose
      (format
       out
       "~&~32/ansi/: Running ~D test~:P ~:[serially~;in parallel: ~:*~D~]:~%~
        ~@[~&;; serial~%~{~&   ~S~}~]~
        ~@[~&;; parallel~%~{~&   ~S~}~]~
        ~@[~&;; serial~%~{~&   ~S~}~]~&"
       :INFO (+ (length prologue) (length parallel-tests) (length epilogue))
       (and parallel-tests (length parallel-tests))
       prologue parallel-tests epilogue))
    (labels ((evaluate (run)
               ;; Evaluates a test-run. Adds failed test to failed-runs.
               (unless (evaluate-test-run run :verbose verbose :output out)
                 (push run failed-runs)))
             (start-run (test &key (parallel parallel))
               ;; Starts the TEST in another thread - returns a promise.
               ;; Create the test-run status object.
               (let* ((run (make-test-run
                            :test test
                            :parallel (and parallel (not (get test 'order)))
                            :timeout (get test 'timeout (default-timeout))
                            :output-stream (make-string-output-stream)))
                      (package (symbol-package test))
                      (name (format nil "~@[~A::~]~A"
                                    (and package (package-name package))
                                    (symbol-name test))))
                 (unless (search "TEST" name :test #'char-equal)
                   (setf name (format nil "~A - TEST" name)))
                 ;; Record all of the test-runs created.
                 (push run all-runs)
                 ;; Call run-test with specified arguments.
                 (flet ((run () (run-test test :run run :debug debug) run))
                   ;; Start the thread that runs the test.
                   (when (and verbose (not parallel))
                     (format
                      out "~&~32/ansi/: Scheduling test: ~A~%" :INFO test))
                   (if threaded
                       (let ((thread (make-thread #'run :name name)))
                         ;; Return a promise to join the thread.
                         (lambda ()
                           (handler-case (join-thread thread)
                             (error (e) (update-test-run run e)))
                           (evaluate run)))
                       (lambda ()
                         (evaluate (run)))))))
             (run-immediate (test)
               ;; Run and immediately join on the thread.
               ;; This synchronizes the execution.
               (funcall (start-run test :parallel nil))))
      (declare (inline evaluate start-run run-immediate))
      (when verbose (separator-line out))
      ;; Execute the schedule.
      ;; Serial tests executed at start before the whole body of tests:
      (map () #'run-immediate prologue)
      ;; Parallel tests evaluate in between.
      (map () #'funcall (mapcar #'start-run parallel-tests))
      ;; Serial tests executed at the end after all of the tests.
      (map () #'run-immediate epilogue)
      ;; Check if any loose thread generated a failed condition.
      (let ((*failed-conditions*
             (with-mutex (*failed-conditions-mutex*)
               (shiftf *failed-conditions* nil))))
        (when *failed-conditions*
          (let ((run (make-test-run :test 'unknown)))
            (test-run-start-time run)
            (update-test-run run)
            (evaluate run)
            (push run all-runs)))))
    (setf *checks-count* 0)
    ;; Failed-tests are in the reverse order.
    (values (nreverse all-runs) (nreverse failed-runs))))

(defun run-tests (&key
                  (debug *debug-unit-tests*)
                  (threaded (unless debug *threaded*))
                  (parallel (and threaded *parallel*))
                  (out *error-output*)
                  (verbose t))
  "Runs all tests.
 Arguments:
  DEBUG    - when non-nil, send each test failure to the debugger
  PARALLEL - when non-nil, the tests may be executed in parallel on multiple threads
  THREADED - when nil, tests are run in the current thread.
  OUT      - stream to send output to
  VERBOSE  - when non-nil, output test test-run lines for each test
 Returns T if all tests succeed."
  (multiple-value-bind (all failed)
      (%run-tests :debug debug :parallel parallel :threaded threaded
                  :out out :verbose verbose)
  (let ((all-count (length all))
        (failed-count (length failed))
        (check-count (loop for test-run in all sum (test-run-checks-count test-run))))
    (when (or verbose debug)
      (format out "~&Run ~D test~:p with ~D check~:p. ~D failed test~:p."
              all-count check-count failed-count))
    (zerop failed-count))))

(defvar *reporting-hooks* nil "User-specified final report functions")
(defun report-tests (tests &key (out *error-output*))
  (dolist (hook *reporting-hooks*)
    (handler-case (funcall hook tests :out out)
      (:no-error (&rest vals) (values-list vals))
      (error (e) (format t "Caught [~A] while trying to run reporting hook" e))))
  (let ((fail-count (count-if #'test-run-error tests))
        (all-count (length tests))
        (checks-count (loop for test-run in tests sum (test-run-checks-count test-run))))
    (cond ((plusp fail-count)
           (format out "~&~31/ansi/: ~A (out of ~A) test~:P failed:~%"
                   :ERROR fail-count all-count)
           (separator-line out)
           (dolist (test-run tests)
             (when (test-run-error test-run)
               (report-failure test-run :out out)))
           fail-count)
          ((zerop all-count)
           (format out "~&~33/ansi/: No tests have been executed.~%" :WARNING)
           -1)
          (t
           (format
            out "~&~32/ansi/: ~D test~:P passed.~:[~*~; Counted ~D check~:p.~]~%"
            :INFO all-count (plusp checks-count) checks-count)
           0))))

(defun run-and-report-tests (&key (out *error-output*) (verbose t))
  "This function is run from the MAIN function and reports the failed tests.
 OUT is the stream to report the test failures.
 VERBOSE will print a report for all tests.
 Returns the number of failed tests.
 Returns -1 if no tests are registered.
 Returns 0 otherwise."
  (prog1
    (report-tests (%run-tests :debug nil :verbose verbose :out out) :out out)
    ;; Cleanup Lisp-gc managed c-objects so asan doesn't complain
    ;; This GC call assumes that cleanups are synchronous with completion of GC,
    ;; but we've seen ASAN complains anyway as it requires lots of other help.
    ;; So probably just remove this after further testing.
    (trivial-garbage:gc :full t)))

(defun deregister-tests (&optional (select :all))
  "Deregister unit-tests. The tests selected will no longer be run by RUN-TESTS or CHECK-TESTS.
 SELECT determines which tests will be removed from the unit-tests list:
   :all        - all tests,
   :package    - tests from the current package,
   :other      - tests from other packages,
   :null       - tests without a package,
   :fail       - tests that fail.
   :pass       - tests that pass.
   <package>   - tests from the package specified.
   <predicate> - tests for which the predicate returns true.
 Returns the remaining registered tests."
  (let ((package *package*)
        (predicate nil))
    (typecase select
      (package  (shiftf package select :package))
      (function (shiftf predicate select :predicate)))
    (labels ((failp (test)
               (test-run-error (run-test test :debug nil)))
             (filter (test)
               (ecase select
                 (:all       t)
                 (:package   (eq (symbol-package test) package))
                 (:predicate (funcall predicate test))
                 (:other     (not (eq (symbol-package test) package)))
                 (:null      (null (symbol-package test)))
                 (:fail      (failp test))
                 (:pass      (not (failp test))))))
       (setf *unit-tests* (delete-if #'filter *unit-tests*)))))
