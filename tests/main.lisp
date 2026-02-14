(defpackage matrix-bot/tests/main
  (:use :cl
        :matrix-bot
        :rove))
(in-package :matrix-bot/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :matrix-bot)' in your Lisp.
(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
