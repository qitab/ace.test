;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Test for ace.test.xml-report.
;;;
;;; cllint: disable=line-length

(defpackage #:ace.test.xml-report-test
  (:use #:common-lisp #:ace.test)
  (:import-from #:ace.test.runner
                #:test-run
                #:make-test-run)
  (:import-from #:ace.test.xml-report
                #:print-tests-report))

(in-package #:ace.test.xml-report-test)

(defun millisec-to-internal-time-units (millisec) "Convert MILLISEC to internal-time-units"
  (ceiling (* millisec internal-time-units-per-second) 1000))

(deftest print-tests-report-no-failure-test ()
  (let* ((run (make-test-run
               :test 'print-tests-report-test
               :error nil
               :trace nil
               :output-text "foo"
               :checks-count 10
               :failed-conditions nil
               :real-time-start 0
               :real-time-stop (millisec-to-internal-time-units 666)))
         (xml (with-output-to-string (s)
                (print-tests-report (list run) s))))

    (check (equal xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<testsuites name=\"xml-report-test\" tests=\"1\">
<testsuite name=\"ACE.TEST.XML-REPORT-TEST\" tests=\"1\" failures=\"0\" errors=\"0\" time=\"0.666\">
  <testcase name=\"PRINT-TESTS-REPORT-TEST\" status=\"run\" classname=\"ACE.TEST.XML-REPORT-TEST\" time=\"0.666\">
  <system-out><![CDATA[foo]]></system-out>
  <properties>
   <property name=\"lisp-function\" value=\"http://cs/PRINT-TESTS-REPORT-TEST\" />
   <property name=\"checks-count\" value=\"10\" />
  </properties>
  </testcase>
</testsuite>
</testsuites>
"))))

(deftest print-tests-report-error-test ()
  (let* ((run (make-test-run
               :test 'print-tests-report-test
               :error (make-condition 'simple-error :format-control "foo~% bar~% baz")
               :trace "Backtrace for: #<SB-THREAD:THREAD \"main thread\" RUNNING {1003557103}>
0: ((LAMBDA NIL :IN SB-DEBUG::FUNCALL-WITH-DEBUG-IO-SYNTAX))
1: (SB-IMPL::CALL-WITH-SANE-IO-SYNTAX #<CLOSURE (LAMBDA NIL :IN SB-DEBUG::FUNCALL-WITH-DEBUG-IO-SYNTAX) {101D7CABEB}>)
2: (SB-IMPL::%WITH-STANDARD-IO-SYNTAX #<CLOSURE (LAMBDA NIL :IN SB-DEBUG::FUNCALL-WITH-DEBUG-IO-SYNTAX) {101D7CABBB}>)
3: (SB-DEBUG:PRINT-BACKTRACE :STREAM NIL :START 0 :FROM :DEBUGGER-FRAME :COUNT NIL :PRINT-THREAD T :PRINT-FRAME-SOURCE NIL :METHOD-FRAME-STYLE NIL)
4: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SB-DEBUG:PRINT-BACKTRACE) #<NULL-LEXENV>)
5: (EVAL (SB-DEBUG:PRINT-BACKTRACE))
6: (SWANK::EVAL-REGION \"(sb-debug:print-backtrace)
\")"
               :checks-count 10
               :failed-conditions nil
               :real-time-start 0
               :real-time-stop (millisec-to-internal-time-units 666)))
         (xml (with-output-to-string (s)
                (print-tests-report (list run) s))))

    (check (equal xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<testsuites name=\"xml-report-test\" tests=\"1\">
<testsuite name=\"ACE.TEST.XML-REPORT-TEST\" tests=\"1\" failures=\"0\" errors=\"1\" time=\"0.666\">
  <testcase name=\"PRINT-TESTS-REPORT-TEST\" status=\"run\" classname=\"ACE.TEST.XML-REPORT-TEST\" time=\"0.666\">
    <error message=\"foo
 bar
 baz\" type=\"SIMPLE-ERROR\">
SIMPLE-ERROR:
foo
 bar
 baz

<![CDATA[Backtrace for: #<SB-THREAD:THREAD \"main thread\" RUNNING {1003557103}>
0: ((LAMBDA NIL :IN SB-DEBUG::FUNCALL-WITH-DEBUG-IO-SYNTAX))
1: (SB-IMPL::CALL-WITH-SANE-IO-SYNTAX #<CLOSURE (LAMBDA NIL :IN SB-DEBUG::FUNCALL-WITH-DEBUG-IO-SYNTAX) {101D7CABEB}>)
2: (SB-IMPL::%WITH-STANDARD-IO-SYNTAX #<CLOSURE (LAMBDA NIL :IN SB-DEBUG::FUNCALL-WITH-DEBUG-IO-SYNTAX) {101D7CABBB}>)
3: (SB-DEBUG:PRINT-BACKTRACE :STREAM NIL :START 0 :FROM :DEBUGGER-FRAME :COUNT NIL :PRINT-THREAD T :PRINT-FRAME-SOURCE NIL :METHOD-FRAME-STYLE NIL)
4: (SB-INT:SIMPLE-EVAL-IN-LEXENV (SB-DEBUG:PRINT-BACKTRACE) #<NULL-LEXENV>)
5: (EVAL (SB-DEBUG:PRINT-BACKTRACE))
6: (SWANK::EVAL-REGION \"(sb-debug:print-backtrace)
\")]]></error>
  <properties>
   <property name=\"lisp-function\" value=\"http://cs/PRINT-TESTS-REPORT-TEST\" />
   <property name=\"checks-count\" value=\"10\" />
  </properties>
  </testcase>
</testsuite>
</testsuites>
"))))

(deftest print-tests-report-failures-test ()
  (let* ((run (make-test-run
               :test 'print-tests-report-test
               :checks-count 10
               :failed-conditions
               (list (make-condition 'simple-error :format-control "foo~% bar~% baz")
                     (make-condition 'simple-warning :format-control "qux~% quax~% quaz"))
               :real-time-start 0
               :real-time-stop (millisec-to-internal-time-units 666)))
         (xml (with-output-to-string (s)
                (print-tests-report (list run) s))))

    (check (equal xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<testsuites name=\"xml-report-test\" tests=\"1\">
<testsuite name=\"ACE.TEST.XML-REPORT-TEST\" tests=\"1\" failures=\"1\" errors=\"0\" time=\"0.666\">
  <testcase name=\"PRINT-TESTS-REPORT-TEST\" status=\"run\" classname=\"ACE.TEST.XML-REPORT-TEST\" time=\"0.666\">
    <failure message=\"foo
 bar
 baz\" type=\"SIMPLE-ERROR\">
SIMPLE-ERROR:
foo
 bar
 baz</failure>
    <failure message=\"qux
 quax
 quaz\" type=\"SIMPLE-WARNING\">
SIMPLE-WARNING:
qux
 quax
 quaz</failure>
  <properties>
   <property name=\"lisp-function\" value=\"http://cs/PRINT-TESTS-REPORT-TEST\" />
   <property name=\"checks-count\" value=\"10\" />
   <property name=\"failed-checks\" value=\"2\" />
  </properties>
  </testcase>
</testsuite>
</testsuites>
"))))
