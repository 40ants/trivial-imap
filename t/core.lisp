(defpackage #:trivial-imap-test/core
  (:use #:cl
        #:rove))
(in-package trivial-imap-test/core)


(deftest test-skip
  (ok (equal (trivial-imap::skip nil 10)
             nil))
  (ok (equal (trivial-imap::skip (list 1 3 4 10 11 12 13) 10)
             (list 11 12 13)))
  (ok (equal (trivial-imap::skip (list 1 3 4 10 11 12 13) 13)
             nil)))
