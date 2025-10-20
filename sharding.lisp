;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; A plug-in for the //list/test:runner sharding the tests.
;;;
;;; This package contains a hook for test runner `MAKE-SCHEDULE' that
;;; shards the tests if specified environment variables are present.
;;;
;;;  cllint: disable=invalid-assert
;;;

(defpackage #:ace.test.sharding
  (:use #:cl #:ace.core)
  (:import-from #:ace.core.os #:getenv)
  (:import-from #:ace.test.runner
                ace.test.runner:order
                ace.test.runner:make-schedule))

(in-package #:ace.test.sharding)

(defun fixed-order-p (test) (integerp (get test 'order)))

(defun nop () (expect :nothing))

(defmethod make-schedule :around (tests &key &allow-other-keys)
;; In the absence of the test sharding environment variables,
;; the test behaves as a normal test.
;;
;;
;; - TEST_TOTAL_SHARDS defines the total number of shards in use
;; - TEST_SHARD_INDEX defines this instance's shard number.
;;   Must honor 0 <= TEST_SHARD_INDEX < TEST_TOTAL_SHARDS
;;
;; If the environment variable TEST_SHARD_STATUS_FILE is set,
;; the test must create this path. Non sharding-compliant tests must not
;; create TEST_SHARD_STATUS_FILE.
;;
;; If set, TEST_SHARD_STATUS_FILE is guaranteed to be outside of TEST_TMPDIR.
;;
;; If no methods are run on a given shard, the binary must exit in success.
;;
;; The sharding function must be stable:
;; Running the same binary with the same shard number and total shard number
;; must execute the same set of test methods each time.
;;
;; The sharding function must be complete:
;; Across all the shards, each test must be run at least once.
;;
;; The sharding function must be a partition:
;; Across all the shards, each test must be run at most once.
;;
  (let ((total-shards (getenv "TEST_TOTAL_SHARDS"))
        (shard-index (getenv "TEST_SHARD_INDEX"))
        (status-file (getenv "TEST_SHARD_STATUS_FILE"))
        prologue parallel epilogue)
    (multiple-value-setq (prologue parallel epilogue)
      (call-next-method))
    (when (and total-shards shard-index)
      (setf total-shards (parse-integer total-shards :junk-allowed t)
            shard-index (parse-integer shard-index :junk-allowed t))
      (when (and total-shards shard-index
                 (< -1 shard-index total-shards)
                 (not (some #'fixed-order-p prologue))
                 (not (some #'fixed-order-p epilogue)))
        ;; Sharding can commence.
        (when status-file
          (open status-file :direction :probe :if-does-not-exist :create))
        ;; Update the test lists.
        (let* ((test-lists `(,prologue ,parallel ,epilogue))
               (lengths (mapcar #'length test-lists))
               (total (reduce #'+ lengths))
               (shard-start 0)
               shard-size shard-rem)
          (declare (dynamic-extent test-lists))
          (setf (values shard-size shard-rem)
                (truncate total total-shards))
          ;; Determine where shard starts.
          (loop :for shard-i :below shard-index :do
            (incf shard-start shard-size)
            (when (< shard-i shard-rem)
              (incf shard-start)))
          ;; The first shard-rem shards are a size larger.
          (when (< shard-index shard-rem)
            (incf shard-size))

          (when (zerop shard-size)
            ;; Just do nothing.
            ;; NOP is required by the framework - otherwise fails.
            (return-from make-schedule (values '(nop) nil nil)))


          ;; Loop over the test lists, and then the tests within each list,
          ;; assigning each test into a different shard round-robin.  Once
          ;; each shard has the same number of tests, the MOD will cause us
          ;; to loop back to the first shard again.  We then only collect
          ;; the tests that are assigned to our SHARD-INDEX.
          (let* ((i 0) ; I gets incremented for each test, without regard to
                       ; which TEST-LIST it is in.
                 (result
                  (loop :for test-list :in test-lists
                        :collect
                        ;; The outer loop essentially filters the input
                        ;; TEST-LISTS.  It will return the same number of
                        ;; sublists as it started with, and each list will
                        ;; contain a subset of the input sublist.
                        (loop :for test :in test-list
                              :for index = (mod i total-shards)
                              :do (incf i)
                              :when (= shard-index index)
                                ;; The inner loop collects up the subset of
                                ;; the tests in TEST-LIST that map to our
                                ;; SHARD-INDEX
                                :collect test))))
            (return-from make-schedule (values-list result))))))
    (if (and shard-index (plusp shard-index))
        ;; All tests run on first shard.
        (values '(nop) nil nil)
        (values prologue parallel epilogue))))
