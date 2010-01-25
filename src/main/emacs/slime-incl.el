
(setq slime-lisp-implementations
      '((ga-sandbox ("@bin.dir@/repl")
                     :init swank-clojure-init
                     :init-function krb-swank-clojure-init)))
