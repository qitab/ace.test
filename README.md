# Lisp Test Runner

![SBCL-Tests](https://github.com/qitab/ace.test/workflows/SBCL-Tests/badge.svg)
[![Gitter](https://badges.gitter.im/qitab/community.svg)](https://gitter.im/qitab/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

This library is a test runner for Bazel `lisp_test` rules. To use it, define a
`lisp_test` rule which has `//ace/test` in its deps:

```build
load("//lisp/devtools/bazel:rules.bzl", "lisp_library", "lisp_test")

lisp_test(
  name = "foo-test",
  srcs = ["foo-test.lisp"],
  deps = [
    ":foo",
    "//ace/test",
  ],
)
```

Then use the `ace.test` package, which defines the `deftest` macro, and use
`expect` or `check` to implement test assertions:

```lisp
(defpackage #:foo-test
  (:use #:common-lisp #:ace.test #:foo))

(in-package #:foo-test)

(deftest test-something ()
  (expect (equal (something) expected-value)))
```

The Bazel [`--test_filter`](https://docs.bazel.build/versions/master/command-line-reference.html) flag can
be used to run a subset of the tests defined in the target. Pass a test name or
a comma-separated list of test names to run.

### Disclaimer: This is not an official Google product.
