(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;(setq ac-delay 0.0)  ;; eclipse uses 500ms
;(define-key ac-completing-map "\e" 'ac-stop)  ;; make Esc hide suggestions
