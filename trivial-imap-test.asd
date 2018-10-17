(defsystem trivial-imap-test
           :author "Alexander Artemenko"
           :license "BSD"
           :class :package-inferred-system
           :pathname "t"
           :depends-on (:trivial-imap
                        "trivial-imap-test/core")
           :description "Test system for trivial-imap."

           :perform (test-op :after (op c)
                             (symbol-call :rove :run c)
                             (clear-system c)))
