(defpackage #:trivial-imap2-test/core
  (:use #:cl
        #:trivial-imap2/core
        #:rove
        #:hamcrest/rove))
(in-package trivial-imap2-test/core)


(deftest test-some-staff
    (testing "Replace this test with real staff."
      (assert-that (foo 1 2)
                   (contains 1 2))))
