;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Test for ace.test.sharding.
;;;

(defpackage #:ace.test.sharding-test
  (:use #:common-lisp #:ace.test)
  (:import-from #:ace.test.runner
                ace.test.runner:make-schedule
                ace.test.runner::*unit-tests*))

(in-package #:ace.test.sharding-test)

;;; Tests 4, 5, and 6 do in parallel
;;; Tests 1, 2, 3 and 7 go in epilogue
;;;
;;; They should get sharded into 5 shards like so:
;;;
;;; Test 4 -> Shard 1
;;; Test 5 -> Shard 2
;;; Test 6 -> Shard 3
;;; Test 1 -> Shard 4
;;; Test 2 -> Shard 5
;;; Test 3 -> Shard 1
;;; Test 7 -> Shard 2
;;;

(deftest test1 :order t ()
  (format t "TEST1 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '() parallel))
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
    (expect (equal '(test4) parallel))
    (expect (equal '(test3) epilogue))))

(deftest test4 :order nil ()
  (format t "TEST4 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test4) parallel))
    (expect (equal '(test3) epilogue))))

(deftest test5 :order nil ()
  (format t "TEST5 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test5) parallel))
    (expect (equal '(test7) epilogue))))

(deftest test6 :order nil ()
  (format t "TEST6 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test6) parallel))
    (expect (equal '() epilogue))))

(deftest test7 :order t ()
  (format t "TEST7 sharded here.~%")
  (multiple-value-bind (prologue parallel epilogue)
      (make-schedule *unit-tests*)
    (expect (equal '() prologue))
    (expect (equal '(test5) parallel))
    (expect (equal '(test7) epilogue))))
