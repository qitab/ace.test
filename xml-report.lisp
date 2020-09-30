;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; A plug-in for the //list/test:runner printing JUnit XML report.
;;;
;;; This package contains a hook for test runner `REPORT-TESTS' that
;;; prints a JUnit XML report from `TEST-RUN' objects to the
;;; XML_OUTPUT_FILE specified in the environment.
;;;

(defpackage #:ace.test.xml-report
  (:use #:common-lisp)
  #+sbcl
  (:import-from #:sb-posix #:getenv)
  (:import-from #:ace.core.string
                #:search-replace)
  (:import-from #:ace.core.macro
                #:function-file-path)
  (:import-from #:ace.test.runner
                #:with-sane-io-syntax
                #:report-tests
                #:test-run
                #:test-run-test
                #:test-run-error
                #:test-run-trace
                #:test-run-output-text
                #:test-run-checks-count
                #:test-run-failed-conditions
                #:test-run-real-time))

(in-package #:ace.test.xml-report)

(defun xml-special-char-p (c)
  "True if `C' is one of the XML characters that need to be escaped."
  (declare (character c))
  (find c '(#\& #\" #\' #\< #\> #\%) :test #'char=))

(defun esc (thing &key (aesthetic t))
  "Return the `THING' as string with all special XML characters escaped.
 If `AESTHETIC' is non-nil, escapes and print readable mode is turned on."
  (declare (boolean aesthetic))
  (let ((string (cond ((stringp thing) thing)
                      (aesthetic (princ-to-string thing))
                      (t         (prin1-to-string thing)))))
    (declare (string string))
    (if (find-if #'xml-special-char-p string)
        (with-output-to-string (out)
          (loop for c across string do
            (case c
              (#\& (write-string "&amp;" out))
              (#\" (write-string "&quot;" out))
              (#\' (write-string "&apos;" out))
              (#\< (write-string "&lt;" out))
              (#\> (write-string "&gt;" out))
              (#\% (write-string "&#37;" out))
              (t   (write-char c out)))))
        string)))

(defun cdata (string)
  "Return a CDATA section with STRING escaped."
  (format nil "<![CDATA[~A]]>" (search-replace "]]>" "]]]]><!CDATA[>" string)))

(defun properties (properties out)
  "Print a list of PROPERTIES to OUT. Each property is a key value list."
  (when properties
    (format out "~&  <properties>~%")
    (dolist (p properties)
      (format out "~&   <property name=\"~A\" value=\"~A\" />~%"
              (esc (first p)) (esc (second p))))
    (format out "~&  </properties>~%")))

(defun test-package-name (test-status)
  "Returns the package name in which the `TEST-STATUS' was defined."
  (let ((package (symbol-package (test-run-test test-status))))
    (and package (package-name package))))

(defun print-condition (condition out &key (as :failure) trace)
  "Prints the CONDITION to OUT as an XML failure or error depending on AS parameter.
 `TRACE' of the stack will be printed if given."
  (ignore-errors
   (write-string
    (with-output-to-string (f)
      (let ((message (esc condition))
            (type (esc (type-of condition) :aesthetic nil)))
        (format f "~&    <~(~A~) message=\"~A\" type=\"~A\">~%" as message type)
        (format f "~A:~%~A" type message)
        (when trace
          (fresh-line f)
          (terpri f)
          (write-string (cdata (string-trim '(#\Newline #\Space #\Tab) trace)) f))
        (format f "</~(~A~)>~%" as)))
    out)))

(defun print-test-case (status &key (out *standard-output*))
  "Print a test case from the test `STATUS' information to the stream `OUT'."
  (with-accessors ((test              test-run-test)
                   (error             test-run-error)
                   (trace             test-run-trace)
                   (checks-count      test-run-checks-count)
                   (failed-conditions test-run-failed-conditions)
                   (output-text       test-run-output-text)
                   (time              test-run-real-time)) status
    (print "QQQ")
    (print test)
    (with-sane-io-syntax
      (let* ((package (symbol-package test))
             (package-name (and package (package-name package)))
             ;; Using kythe might be better, but I could not find any
             ;; way to construct a link using language server protocol.
             (codesearch-link
              #+google3 (format nil "http://cs/~@[f:~A%20~]~A" (function-file-path test)
                                (symbol-name test))))
        (format
         out "~&  <testcase name=\"~A\" status=\"run\" classname=\"~A\" time=\"~D\">~%"
         (esc test) (esc package-name) (or time -1))
        (dolist (failure failed-conditions)
          (print-condition failure out :as :failure))
        (when error
          (print-condition error out :as :error :trace trace))
        (when (plusp (length output-text))
          (format out "~&  <system-out>~A</system-out>~%" (cdata output-text)))
        (properties `(,@(when codesearch-link
                          `(("lisp-function" ,codesearch-link)))
                      ("checks-count" ,checks-count)
                      ,@(when failed-conditions
                          `(("failed-checks" ,(length failed-conditions)))))
                    out))
      (format out "~&  </testcase>~%"))))

(defun print-test-cases (test-cases &key (out *standard-output*))
  "Prints the `TEST-CASES' to the `OUT' stream."
  (dolist (test test-cases)
    (print-test-case test :out out)))

(defun print-test-suite (name test-cases &key
                                         (print-function #'print-test-cases)
                                         (out *standard-output*))
  "Print `TEST-CASES' to the stream `OUT' as a JUnit test suite with the NAME.
   The `TEST-CASES' are a list of test.runner STATUS objects.
   Calls `PRINT-FUNCTION' on the cases after printing the test suite header and before the footer."
  (with-sane-io-syntax
    (let ((failure-count (count-if #'test-run-failed-conditions test-cases))
          (error-count
           (count-if (lambda (s)
                       (and (test-run-error s) (not (test-run-failed-conditions s))))
                     test-cases))
          (total-time (loop for s in test-cases sum (test-run-real-time s))))
      (format
       out "~&<testsuite name=\"~A\" tests=\"~D\" failures=\"~D\" errors=\"~D\" time=\"~D\">~%"
       (esc name) (length test-cases) failure-count error-count total-time)
      (funcall print-function test-cases :out out)
      (format out "~&</testsuite>~%"))))

(defun group-and-print-test-cases (test-cases &key
                                             (key #'test-package-name)
                                             (test #'eq)
                                             (default "unknown")
                                             (print-function #'print-test-suite)
                                             (out *standard-output*))
  "Groups the `TEST-CASES' and calls `PRINT-FUNCTION' on the `OUT' stream.
 The group function used is specified by the `KEY' parameter.
 `TEST' is used to compare the keys. `DEFAULT' is used if `KEY' returns NIL."
  (declare (function print-function test key))
  (let ((table (make-hash-table :test test)))
    (dolist (status test-cases)
      (push status (gethash (or (funcall key status) default) table)))
    (maphash (lambda (key test-cases)
               (funcall print-function key test-cases :out out))
             table)))

(defun print-tests-report (test-cases out)
  "Prints the TEST-CASES' status objects to the OUT stream as a JUnit XML test report."
  (format out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
  (let ((program (or #+sbcl (pathname-name (first sb-unix::*posix-argv*)) "")))
    (format out "~&<testsuites name=\"~A\" tests=\"~D\">~%"
            (esc program) (length test-cases)))
  (group-and-print-test-cases test-cases :key #'test-package-name :out out)
  (format out "~&</testsuites>~%"))

(defmethod report-tests dump-junit-xml-output (test-cases &key &allow-other-keys)
  (let ((xml-output (getenv "XML_OUTPUT_FILE")))
    ;; This variable is specified here:
    
    ;; The file will be augmented by blaze runner with additional information after the test
    ;; finishes.
    ;; Additional info can be found here:
    
    (when xml-output
      (with-open-file (out xml-output :direction :output
                                      :element-type 'character
                                      :external-format :utf-8
                                      :if-exists :supersede)
        (print-tests-report test-cases out)))))
