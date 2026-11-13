;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;
;;; Test the unit test package...
;;;

(cl:defpackage #:ace.test-test
  (:use #:common-lisp #:ace.test)
  (:import-from #:ace.test.runner
                #:*failed-conditions*
                #:*unit-tests*
                #:%run-tests
                #:make-schedule))

(cl:in-package #:ace.test-test)

;;;  CHECK and EXPECT are tested with this functionality.
;;;  cllint: disable=invalid-assert

(deftest signals-test ()
  (assert (signals warning
            (warn "This warning should be detected")
            (error "The warning has not been detected")))

  (assert (signals error
            (assert (signals simple-condition 'no-op))))

  (assert (signals simple-error
            (error "This error should be detected"))))

(deftest expect-error-test ()
  (assert (null *failed-conditions*))
  (expect-error (error "This error should be detected"))
  (assert (null *failed-conditions*))

  (expect-error (error "An expected error.")))

(defun minus (a b) (- a b))

(defun plus (a b) (+ a b))

(deftest with-mock-functions-test ()
  (with-mock-functions
      ((minus (load-time-value #'plus))
       (plus (lambda (a b) (* a b))))
    (expect (= 6 (minus 3 3)))
    (expect (= 9 (plus 3 3)))))

(deftest with-mock-functions-test2 ()
  (with-mock-functions
      ((plus (load-time-value #'minus))
       (minus (lambda (a b) (* a b))))
    (expect (= 0 (plus 3 3)))
    (expect (= 9 (minus 3 3)))))

(defvar *foo*)
(defun (setf foo) (v) (setf *foo* v))

(deftest with-mock-functions-test3 ()
  "Test that with-mock-functions can mock (setf ...) accessors."
  (let (*foo* bar)
    (with-mock-functions (((setf foo) (lambda (v) (setf bar v))))
      (setf (foo) :bar)
      (expect (null *foo*))
      (expect (eq :bar bar)))))

(deftest with-mock-functions-test4 ()
  (let ((real-plus (symbol-function 'plus))
        (real-minus (symbol-function 'minus)))
    (with-mock-functions
        ((minus #'plus)
         (plus (lambda (a b) (funcall real-minus (* a b) a))))
      (expect (= 6 (minus 3 3)))
      (expect (= 0 (funcall real-minus 3 3)))
      (expect (= 6 (plus 3 3)))
      (expect (= 5 (funcall real-plus 2 3))))))

(defvar *bar*)
(defun bar () *bar*)
(defun (setf bar) (v) (setf *bar* v))

(deftest letf*-test ()
  (let ((a 'a) *bar* (c 'c))
    (letf* ((a 1)
            ((bar) 4)
            (c 3))
      (expect (= 1 a))
      (expect (= 4 *bar*))
      (expect (= 3 c)))
    (expect (eq a 'a))
    (expect (eq c 'c))
    (expect (not *bar*))))

(deftest assert-failure-test ()
  ;; Intentionally errors out.
  (check (not "EVER-PASSES")))

(defvar *fixture-events* nil)

(define-test-fixture
  :setup (push :setup-1 *fixture-events*)
  :teardown (push :teardown-1 *fixture-events*))

(deftest first-fixture-test ()
  (when *fixture-events*
    (expect (equal *fixture-events* '(:setup-1)))))

(define-test-fixture
  :setup (push :setup-2 *fixture-events*)
  :teardown (push :teardown-2 *fixture-events*))

(deftest second-fixture-test ()
  (when *fixture-events*
    (expect (equal *fixture-events* '(:setup-2 :teardown-1 :setup-1)))))

(defun report-unknown-failures ()
  (let ((dev/null (make-broadcast-stream))
        (*failed-conditions* nil)
        (*unit-tests* nil))
    (expect nil "Expect failure outside of deftest")
    (assert *failed-conditions*)
    (assert (= 1 (ace.test.runner:run-and-report-tests
                  :out dev/null :verbose nil)))))

(defun %main ()
  (assert (member 'signals-test *unit-tests*))
  (assert (member 'expect-error-test *unit-tests*))
  (assert (member 'assert-failure-test *unit-tests*))
  (assert (member 'first-fixture-test *unit-tests*))
  (assert (member 'second-fixture-test *unit-tests*))

  (format t "RT:~{~&  ~A~%~}" (reverse *unit-tests*))

  (let ((unit-tests *unit-tests*)
        (unit-tests-cpy (copy-list *unit-tests*)))
    (assert (eq unit-tests *unit-tests*))
    (assert (equal unit-tests-cpy *unit-tests*)))

  ;; Need to call all the test here since not using the runner.

  (signals-test)
  (expect-error-test)
  (assert (signals error
            (assert-failure-test)))
  (with-mock-functions-test)
  (with-mock-functions-test2)
  (with-mock-functions-test3)
  (with-mock-functions-test4)
  (letf*-test)
  (report-unknown-failures)
  (first-fixture-test)
  (second-fixture-test)

  (setf *fixture-events* nil)
  (multiple-value-bind (all failed) (%run-tests :debug nil :verbose t)
    (declare (list all failed))
    (assert (equal *fixture-events* '(:teardown-2 :setup-2 :teardown-1 :setup-1)))
    (let ((all-count (length all))
          (fail-count (length failed)))
      (assert (= all-count 10))
      (assert (= fail-count 1)))))
