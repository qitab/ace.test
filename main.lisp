;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Default main for unit tests.
;;; cl-user::main is the default main for both the lisp_test and lisp_binary build rules.
;;;
;;; cllint:disable=prefer-logging
;;;

(defpackage :ace.test.main
  (:use :cl)
  #+bordeaux-threads (:import-from #:bordeaux-threads #:make-thread #:all-threads)
  (:local-nicknames #+google3 (#:flag #:ace.flag)))

(in-package :ace.test.main)

;;; Compatibility shims
#+(and sbcl (not bordeaux-threads))
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute) (import '(sb-thread:make-thread)))
  (defun all-threads () (sb-thread:list-all-threads)))

(defun start-timeout-watcher ()
  "Runs a watcher for TIMEOUT minus 5 sec. and prints stack traces if not dead."
  (let ((timeout (ace.test.runner:default-timeout)))
    (when (and timeout (> timeout 5))
      (flet ((timeout-watcher ()
               (sleep (- timeout 5))
               (format *error-output* "INFO: The test is about to timeout.~%")
               #+sbcl
               (let ((*print-pretty* nil))
                 (dolist (pair (sb-debug:backtrace-all-threads))
                   ;; No need for a backtrace of the timeout watcher
                   (unless (eq (car pair) sb-thread:*current-thread*)
                     (format *debug-io* "~&Backtrace for ~A:~%~A~%" (car pair) (cdr pair)))))))
        (make-thread #'timeout-watcher :name "Timeout-Watcher")))))

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
  (unless (zerop (ace.test.runner:run-and-report-tests))
    (exit :status -1))
  (format *error-output* "INFO: Exiting with ~D thread~:p remaining.~%" (length (all-threads)))
  (exit :timeout 10))
