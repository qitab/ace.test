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
  :serial t
  :components
  #.(loop for f in *files* collect `(:file ,f)))
