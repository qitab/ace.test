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
  (:import-from #:ace.core.os #:getenv)
  (:import-from #:ace.core.string
                #:search-replace)
  (:import-from #:ace.test.runner
                #:with-sane-io-syntax
                #:test-run
                #:test-run-test
                #:test-run-error
                #:test-run-trace
                #:test-run-output-text
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

(defun function-file-path (function-name)
  "Return the namestring of the file containing the source code for FUNCTION-NAME"
  (let ((fun (and (fboundp function-name) (fdefinition function-name))))
    (when fun
      #+sbcl
      (let ((di (sb-kernel:%code-debug-info (sb-kernel:fun-code-header fun))))
        (sb-c::debug-source-namestring (sb-c::compiled-debug-info-source di))))))

(defun print-test-case (status out)
  "Print a test case from the test `STATUS' information to the stream `OUT'."
  (with-accessors ((test              test-run-test)
                   (error             test-run-error)
                   (trace             test-run-trace)
                   (failed-conditions test-run-failed-conditions)
                   (output-text       test-run-output-text)
                   (time              test-run-real-time)) status
    (with-sane-io-syntax
      (let* ((package (symbol-package test))
             (package-name (and package (package-name package)))
             ;; Using kythe might be better, but I could not find any
             ;; way to construct a link using language server protocol.
             (codesearch-link
              #+google3 (format nil "http://cs/~@[f:~A%20~]~A" (function-file-path test)
                                (symbol-name test))))
        (format
         out "~&  <testcase name=\"~A\" status=\"run\" classname=\"~A\" time=\"~F\">~%"
         (esc test) (esc package-name) (or time -1))
        (dolist (failure failed-conditions)
          (print-condition failure out :as :failure))
        (when error
          (print-condition error out :as :error :trace trace))
        (when (plusp (length output-text))
          (format out "~&  <system-out>~A</system-out>~%" (cdata output-text)))
        (properties `(,@(when codesearch-link
                          `(("lisp-function" ,codesearch-link)))
                      ,@(when failed-conditions
                          `(("failed-checks" ,(length failed-conditions)))))
                    out))
      (format out "~&  </testcase>~%"))))

(defun print-test-suite (name test-cases out)
  "Print `TEST-CASES' to the stream `OUT' as a JUnit test suite with the NAME."
  (with-sane-io-syntax
    (let ((failure-count (count-if #'test-run-failed-conditions test-cases))
          (error-count
           (count-if (lambda (s)
                       (and (test-run-error s) (not (test-run-failed-conditions s))))
                     test-cases))
          (total-time (loop for s in test-cases sum (test-run-real-time s))))
      (format
       out "~&<testsuite name=\"~A\" tests=\"~D\" failures=\"~D\" errors=\"~D\" time=\"~F\">~%"
       (esc name) (length test-cases) failure-count error-count total-time)
      (dolist (test test-cases)
        (print-test-case test out))
      (format out "~&</testsuite>~%"))))

(defun print-tests-report (test-cases out)
  "Prints the TEST-CASES' status objects to the OUT stream as a JUnit XML test report."
  (format out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
  (let ((program (or #+sbcl (pathname-name (first sb-unix::*posix-argv*)) "")))
    (format out "~&<testsuites name=\"~A\" tests=\"~D\">~%"
            (esc program) (length test-cases)))
  (let ((table (make-hash-table :test #'eq)))
    (dolist (status test-cases)
      (let ((package (symbol-package (test-run-test status))))
        (push status (gethash (or (and package (package-name package)) "unknown") table))))
    (maphash (lambda (key cases)
               (print-test-suite key cases out))
             table))
  (format out "~&</testsuites>~%"))

(defun dump-junit-xml-output (test-cases &key &allow-other-keys)
  (let ((xml-output (getenv "XML_OUTPUT_FILE")))
    ;; This variable is specified here:
    
    ;; The file will be augmented by blaze runner with additional information after the test
    ;; finishes.
    ;; Additional info can be found here:
    
    (when xml-output
      (ignore-errors (delete-file xml-output))
      (with-open-file (out xml-output :direction :output
                                      :element-type 'character
                                      :external-format :utf-8)
        (print-tests-report test-cases out)))))
(pushnew 'dump-junit-xml-output ace.test.runner:*reporting-hooks*)
