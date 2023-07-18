;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Test for ace.test.sharding.
;;;

(defpackage #:ace.test.sharding-test
  (:use #:ace.test #:common-lisp)
  (:import-from #:ace.test.runner
                ace.test.runner:make-schedule
                ace.test.runner::*unit-tests*))

(in-package #:ace.test.sharding-test)

(deftest test1 :order t ()
  (format t "TEST1 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test6) parallel))
    (expect (equal '(test1) epilogue))))

(deftest test2 :order t ()
  (format t "TEST2 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '() parallel))
    (expect (equal '(test2) epilogue))))

(deftest test3 :order t ()
  (format t "TEST3 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '() parallel))
    (expect (equal '(test3) epilogue))))

(deftest test4 :order nil ()
  (format t "TEST4 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test4 test5) parallel))
    (expect (equal '() epilogue))))

(deftest test5 :order nil ()
  (format t "TEST5 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test4 test5) parallel))
    (expect (equal '() epilogue))))

(deftest test6 :order nil ()
  (format t "TEST6 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test6) parallel))
    (expect (equal '(test1) epilogue))))

(deftest test7 :order t ()
  (format t "TEST7 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '() parallel))
    (expect (equal '(test7) epilogue))))

