(defsystem trivial-imap2-test
           :author "Alexander Artemenko"
           :license "BSD"
           :class :package-inferred-system
           :pathname "t"
           :depends-on (:trivial-imap2
                        "trivial-imap2-test/core")
           :description "Test system for trivial-imap2"

           :perform (test-op :after (op c)
                             (symbol-call :rove :run c)
                             (clear-system c)))
