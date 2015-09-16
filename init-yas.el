;; Bootstrap
(when (version<= emacs-version "24.2")
    (require 'cl-lib))
(defvar yas/el-path (concat (file-name-as-directory "..") "yasnippet"))
(defvar yas/snippets-root (concat (file-name-directory yas/el-path)
				  "snippets"))  ; relative to yasnippet.el

(require 'yasnippet yas/el-path)
(load (concat (file-name-as-directory yas/snippets-root) "functions.el"))

;; Yasnippet configuration
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
