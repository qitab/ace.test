;;; A plug-in for the //list/test:runner sharding the tests.
;;;
;;; This package contains a hook for test runner `MAKE-SCHEDULE' that
;;; shards the tests if specified environment variables are present.
;;;
;;;  cllint: disable=invalid-assert
;;;

(defpackage #:ace.test.sharding
  (:use #:cl #:ace.core)
  #+sbcl
  (:import-from #:sb-posix #:getenv)
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

          (loop
            :for list.rest :on test-lists
            :for len :in lengths
            :do
            (cond ((>= shard-start len)
                   ;; Skip.
                   (setf (car list.rest) nil)
                   (decf shard-start len))
                  ((< (+ shard-start shard-size) len)
                   ;; Found the rest of the shard in this list.
                   (setf (car list.rest)
                         (subseq (car list.rest)
                                 shard-start
                                 (+ shard-start shard-size))
                         shard-start 0
                         shard-size 0))
                  (t
                   ;; Found a part of the shard in this list.
                   (decf shard-size (- len shard-start))
                   (setf (car list.rest)
                         (nthcdr shard-start (car list.rest))
                         shard-start 0))))
          (assert (zerop shard-start))
          (assert (zerop shard-size))
          (return-from make-schedule (values-list test-lists)))))
    (if (and shard-index (plusp shard-index))
        ;; All tests run on first shard.
        (values '(nop) nil nil)
        (values prologue parallel epilogue))))
