(add-to-list 'load-path "~/.emacs.d/site-lisp")
(load "~/.emacs.d/init-local")
(load "~/.emacs.d/init-ac")
; use a separate file for Customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Key bindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'next-buffer)

;; Core settings
; enable auto revert (reload)
(global-auto-revert-mode 1)
; always end a file with a newline
(setq require-final-newline 'query)
; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; jump to the beginning of matched string in I-search when C-v is pressed
(define-key isearch-mode-map (kbd "C-v") 'my-isearch-forward-to-beginning)
    (defun my-isearch-forward-to-beginning ()
      "Do a forward search and jump to the beginning of the search-term."
      (interactive)
      (isearch-repeat 'forward)
      (goto-char isearch-other-end))

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(require 'unbound)

;; C/C++
; Set Google Style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Octave / Matlab
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))
