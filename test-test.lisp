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
                #:*checks-count*
                #:*failed-conditions*
                #:*unit-tests*
                #:%run-tests
                #:make-schedule
                #:order
                #:sort-tests))

(cl:in-package #:ace.test-test)

;;;  CHECK and EXPECT are tested with this functionality.
;;;  cllint: disable=invalid-assert

(deftest signalsp-test :order 1 ()
  (assert (signalsp warning
            (warn "This warning should be detected")
            (error "The warning has not been detected")))

  (assert-error
    (assert (signalsp simple-condition 'no-op)))

  (assert (signalsp simple-error
            (error "This error should be detected"))))

(deftest assert-error-test :order t ()
  (assert-error (error "This error should be detected"))

  (assert-error (assert-error 'no-op)))

(deftest expect-error-test :order -1 ()
  (assert (null *failed-conditions*))
  (expect-error (error "This error should be detected"))
  (assert (null *failed-conditions*))

  (expect-error (assert-error 'no-op))

  (expect-error (error "An expected error.")))

(defun minus (a b) (- a b))

(defun plus (a b) (+ a b))

(deftest with-mock-functions-test :order nil ()
  (with-mock-functions
      ((minus #'plus)
       (plus (lambda (a b) (* a b))))
    (expect (= 6 (minus 3 3)))
    (expect (= 9 (plus 3 3)))))

(deftest with-mock-functions-test2 :order nil ()
  (with-mock-functions
      ((plus #'minus)
       (minus (lambda (a b) (* a b))))
    (expect (= 0 (plus 3 3)))
    (expect (= 9 (minus 3 3)))))

(defvar *foo*)
(defun (setf foo) (v) (setf *foo* v))

(deftest with-mock-functions-test3 :order nil ()
  "Test that with-mock-functions can mock (setf ...) accessors."
  (let (*foo* bar)
    (with-mock-functions (((setf foo) (lambda (v) (setf bar v))))
      (setf (foo) :bar)
      (expect (null *foo*))
      (expect (eq :bar bar)))))

(deftest with-mock-functions-test4 :order nil ()
  (with-mock-functions
      ((minus #'plus real-minus)
       (plus (lambda (a b) (real-minus (* a b) a)) real-plus))
    (expect (= 6 (minus 3 3)))
    (expect (= 0 (real-minus 3 3)))
    (expect (= 6 (plus 3 3)))
    (expect (= 5 (real-plus 2 3)))))

(defvar *bar*)
(defun bar () *bar*)
(defun (setf bar) (v) (setf *bar* v))

(deftest letf*-test :order nil  ()
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

(deftest assert-failure-test :order nil ()
  ;; Intentionally errors out.
  (check (not "EVER-PASSES")))

(defmacro accepts-string (a)
  (check-type a string)
  `(format nil "*~A*" ,a))

(deftest assert-macro-error-test :order nil ()
  (assert-macro-error (accepts-string 10))
  (assert-error
    (assert-macro-error (accetps-string "10"))))

(deftest expect-macro-error-test :order nil ()
  (assert (null *failed-conditions*))
  ;; Intentionally errors out.
  (expect-macro-error (accepts-string "10"))
  (assert (= 1 (length *failed-conditions*)))
  (expect-macro-error (accepts-string 10))
  ;; Retained the value.
  (assert (= 1 (length *failed-conditions*)))
  (setf *failed-conditions* nil))

(deftest parse-deftest-options-test :order nil ()
  (multiple-value-bind (order timeout args body)
      (ace.test::parse-deftest-options
       '(:timeout 5 :order 3 (&optional foo bar baz)
         "a docstring"
         (expect (= 0 0))))
    (expect (= timeout 5))
    (expect (= order 3))
    (expect (equal args '(&optional foo bar baz)))
    (expect (equal body '("a docstring" (expect (= 0 0)))))))

(defun report-unknown-failures ()
  (let ((dev/null (make-broadcast-stream))
        (*checks-count* 0)
        (*failed-conditions* nil)
        (*unit-tests* nil))
    (expect nil "Expect failure outside of deftest")
    (assert (> *checks-count* 0))
    (assert *failed-conditions*)
    (assert (= 1 (ace.test.runner:run-and-report-tests
                  :out dev/null :verbose nil)))))

(defun %main ()
  (assert (member 'signalsp-test *unit-tests*))
  (assert (member 'expect-error-test *unit-tests*))
  (assert (member 'assert-error-test *unit-tests*))
  (assert (member 'assert-failure-test *unit-tests*))
  (assert (member 'expect-macro-error-test *unit-tests*))
  (assert (member 'assert-macro-error-test *unit-tests*))
  (assert (member 'parse-deftest-options-test *unit-tests*))

  (assert (get 'assert-error-test 'order))

  (format t "RT:~{~&  ~A~%~}" (reverse *unit-tests*))
  (format t "ST:~{~&  ~A~%~}" (sort-tests (reverse *unit-tests*)))
  (format t "TO: ~A~%" (mapcar (lambda (test) (get test 'order))
                               (sort-tests (reverse *unit-tests*))))

  (let ((unit-tests *unit-tests*)
        (unit-tests-cpy (copy-list *unit-tests*)))

    (assert (equal (sort-tests (reverse *unit-tests*))
                   '(expect-error-test ; -1
                     with-mock-functions-test
                     with-mock-functions-test2
                     with-mock-functions-test3
                     with-mock-functions-test4
                     letf*-test
                     assert-failure-test
                     assert-macro-error-test
                     expect-macro-error-test
                     parse-deftest-options-test
                     signalsp-test     ; 1
                     assert-error-test))) ; t

    (multiple-value-bind (prologue middle epilogue)
        (make-schedule *unit-tests*)
      (assert (equal '(expect-error-test) prologue)) ; -1
      (assert (equal '(with-mock-functions-test
                       with-mock-functions-test2
                       with-mock-functions-test3
                       with-mock-functions-test4
                       letf*-test
                       assert-failure-test
                       assert-macro-error-test
                       expect-macro-error-test
                       parse-deftest-options-test)
                     middle))
      (assert (equal '(signalsp-test     ; 1
                       assert-error-test) ; t
                     epilogue)))

    (assert (eq unit-tests *unit-tests*))
    (assert (equal unit-tests-cpy *unit-tests*)))

  ;; Need to call all the test here since not using the runner.

  (signalsp-test)
  (expect-error-test)
  (assert-error-test)
  (expect-macro-error-test)
  (assert-macro-error-test)
  (assert-error
    (assert-failure-test))
  (with-mock-functions-test)
  (with-mock-functions-test2)
  (with-mock-functions-test3)
  (with-mock-functions-test4)
  (letf*-test)
  (report-unknown-failures)
  (parse-deftest-options-test)

  (multiple-value-bind (all failed) (%run-tests :debug nil :verbose t)
    (declare (list all failed))
    (let ((all-count (length all))
          (fail-count (length failed)))
      (assert (= all-count 12))
      (assert (= fail-count 1)))))
