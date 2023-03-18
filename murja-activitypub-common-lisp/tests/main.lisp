(defpackage murja-activitypub-common-lisp/tests/main
  (:use :cl
        :murja-activitypub-common-lisp
        :rove))
(in-package :murja-activitypub-common-lisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :murja-activitypub-common-lisp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
