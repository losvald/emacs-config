(setq my/packages '(
		    auctex
		    auto-complete
		    flycheck
		    go-autocomplete
		    go-mode
		    image+
		    js2-mode
		    nlinum
		    nodejs-repl
		    sbt-mode
		    skewer-mode
		    ))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package my/packages)
  (unless (package-installed-p package)
    (package-install package)))
