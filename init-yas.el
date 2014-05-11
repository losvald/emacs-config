(when (version<= emacs-version "24.2")
    (require 'cl-lib))

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (interactive "P")
  (mapconcat 'identity (mapcar
			'(lambda (word) (if (or (> (length word) 2)
						(member word '("go" "or")))
					    (capitalize (downcase word))
					  (upcase word)))
			(split-string s "_")) ""))

(require 'yasnippet "../yasnippet")
(yas-global-mode 1)
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c C-n")
  '(lambda()
     (interactive)
     (if (equal major-mode 'coq-mode)	; resolve conflicts in coq-mode
	 (if (and yas--active-field-overlay
		  (overlay-buffer yas--active-field-overlay))
	     (yas-next-field)
	   (proof-assert-next-command-interactive))
       (yas-next-field))))
(define-key yas-minor-mode-map (kbd "C-c C-p") 'yas-prev-field)
(setq yas-trigger-key nil)
(define-key yas-minor-mode-map (kbd "C-c C-SPC") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-c SPC") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-c C-i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-c i") 'yas-insert-snippet)
(setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt))

(setq auto-mode-alist
      (cons '("~/.emacs.d/snippets/*" . snippet-mode) auto-mode-alist))
