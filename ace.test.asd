(defparameter *files*
  '("runner"
    "main"
    "sharding"
    "test"
    "xml-report"))

#+sbcl (require :sb-introspect)

(defsystem ace.test
  :name "Ace Lisp test Libraries"
  :description "Common utilities used at Ace for Lisp and linked into most Ace Lisp binaries."
  :long-description
  "The ACE.test library contains following modules:
 ace.test - is a summary package that can be used as a namespace,
  .main - add default main for testing,
  .runner - the test runner,
  .sharder - utilities for sharding the tests.,
  .test - simple utils to define unit tests,
  .etc - a plug-in for the //list/test:runner printing JUnit XML report"
  :version "1.0"
  :author "Lisp Community"
  :license "MIT"
  :depends-on (bordeaux-threads closer-mop trivial-garbage ace.core)
  :in-order-to ((test-op (test-op :ace.test/tests)))
  :serial t
  :components
  #.(loop for f in *files* collect `(:file ,f)))

(defsystem :ace.test/tests
  :name "ace.test tests"
  :version "1.0"
  :licence "MIT"
  :description      "Test code for ace.test"
  :long-description "Test code for ace.test"
  :depends-on (:ace.core)
  :serial t
  :pathname ""
  :components
  ((:file "test-test"))
  :perform (test-op
            (o c)
            (uiop:symbol-call '#:ace.test-test
                              '#:run-tests)))
