;;; Default main for unit tests.
;;; cl-user::main is the default main for both the lisp_test and lisp_binary build rules.
;;;
;;; cllint:disable=prefer-logging
;;;

(google.core.package:defpackage* :google.test.main
  (:use :cl)
  (:use-alias #:google.core.thread
              #:google.flag))

(in-package :google.test.main)

(defun start-timeout-watcher ()
  "Runs a watcher for TIMEOUT minus 5 sec. and prints stack traces if not dead."
  (let ((timeout (google.test.runner:timeout)))
    (when (and timeout (> timeout 5))
      (flet ((timeout-watcher ()
               (sleep (- timeout 5))
               (format *error-output* "INFO: The test is about to timeout.~%")
               (thread:print-backtraces)))
        (thread:make-thread #'timeout-watcher :name "Timeout-Watcher")))))

#+google3
(flag:define ace.test.runner::*parallel* t
   "Run tests in parallel (default)."
   :name "parallel-lisp-tests"
   :type boolean
   :def nil)

(defun exit (&key (status 0) (timeout 60) abort)
  "Exit with STATUS, waiting at most TIMEOUT seconds for other threads.
If ABORT is true, the process exits recklessly without cleaning up."
  (declare (ignorable timeout abort))
  #+sbcl (sb-ext:exit :code status :abort abort :timeout timeout)
  #+ccl (ccl:quit status)
  #+clisp (ext:quit status)
  #+cmu (unix:unix-exit status)
  #+abcl (ext:quit :status status)
  #+allegro (excl:exit status :quiet t)
  (assert nil () "Aborting process using an ASSERT failure."))

(defun cl-user::main ()
  "Default main for unit tests."
  ;; TODO(czak): Fix the issues with InitGoogle.
  #+google3
  (google:init (flag:parse-command-line :args (append (flag:command-line)
                                                                   '("--logtostderr"))))
  (start-timeout-watcher)
  (unless (zerop (google.test.runner:run-and-report-tests))
    (exit :status -1))
  (format *error-output* "INFO: Exiting with ~D thread~:p remaining.~%"
          (length (thread:all-threads)))
  (exit :timeout 10))
