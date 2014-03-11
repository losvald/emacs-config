(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-source-yasnippet nil)
;; (setq ac-delay 0.0)  ;; eclipse uses 500ms
;; (setq ac-auto-show-menu nil
;;       ac-use-quick-help nil)
;; (define-key ac-completing-map "\e" 'ac-stop)  ;; make Esc hide suggestions

;; Fix some key bindings in ac completions. Using RET when a
;; completion is offered is not usually intended to complete (use
;; TAB for that), but done while typing and the inputer is considere
;; complete, with the intent to simply leave it as is and go to the
;; next line. Much like space will not complete, but leave it as is
;; and insert a space.
(define-key ac-completing-map (kbd "RET") nil)
(define-key ac-completing-map (kbd "<return>") nil)

;; Override default bindings for C-c C-n/p to navigate over yasnippet fields
;; when auto-completing; otherwise those bindings don't work for the 1st time
(define-key ac-completing-map (kbd "C-c C-n")
  (lexical-let ((command (key-binding (kbd "C-c C-n")))) ; remember command
    '(lambda ()
       (interactive)
       (when (and yas--active-field-overlay
		  (overlay-buffer yas--active-field-overlay))
	 (yas-next-field)))))		; TODO invoke default command
(define-key ac-completing-map (kbd "C-c C-p")
  (lexical-let ((command (key-binding (kbd "C-c C-p")))) ; remember command
    '(lambda ()
       (interactive)
       (when (and yas--active-field-overlay
		  (overlay-buffer yas--active-field-overlay))
	 (yas-prev-field)))))		; TODO invoke default command
