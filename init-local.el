(load "~/.emacs.d/init-auctex")

(require 'python)

;; CScope
(require 'xcscope)
(cscope-setup)

;; Scala-mode2 (TODO: move)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/scala-mode2")
(require 'scala-mode2)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/sbt-mode")
(require 'sbt-mode)
