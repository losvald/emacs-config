(add-to-list 'load-path "~/.emacs.d/site-lisp")
(load "~/.emacs.d/init-local")
(load "~/.emacs.d/init-ui.el")
(load "~/.emacs.d/init-spell.el")
(load "~/.emacs.d/init-yas") ;; should be loaded before ac
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
(defun my/bindkey-recompile ()
  "Bind C-c C-c to `recompile'."
  (local-set-key (kbd "C-c C-c") 'recompile))
(add-hook 'c-mode-common-hook
	  (lambda()
	    (my/bindkey-recompile)
	    (c-set-offset 'inextern-lang 0)  ; don't indent "extern" blocks
	    ))
(add-to-list 'auto-mode-alist
	     '(".scratch\\.cpp" . (lambda () (load "~/.emacs.d/scratch-cpp"))))

;; This hack fixes indentation for C++11's "enum class" in Emacs.
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class/6550361#6550361

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1) (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem)) 0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem)) '- '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
	       '(statement-cont . align-enum-class-closing-brace)))

;; (add-hook 'c++-mode-hook 'fix-enum-class)

;; Shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Dot / Graphviz
(load-file "~/.emacs.d/site-lisp/graphviz-dot-mode.el")

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Octave / Matlab
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;; Python

(require 'python)

; Python indentation
(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
  they line up with the line containing the corresponding opening bracket."
(save-excursion
  (beginning-of-line)
  (let ((syntax (syntax-ppss)))
    (if (and (not (eq 'string (syntax-ppss-context syntax)))
             (python-continuation-line-p)
             (cadr syntax)
             (skip-syntax-forward "-")
             (looking-at "\\s)"))
        (progn
          (forward-char 1)
          (ignore-errors (backward-sexp))
          (setq ad-return-value (current-indentation)))
      ad-do-it))))

(add-hook 'python-mode-hook
    (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)
        (ad-activate 'python-calculate-indentation)))

;; R
(autoload 'R-mode "ess-site.el" "" t)
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    ;; resolve key binding conflicts (C-c C-n is yas-next-field)
	    (define-key ess-mode-map (kbd "C-c C-m") 'ess-eval-line-and-step)
	    ;; start emacs server (needed for edit, fix)
	    (require 'server)
	    (unless (server-running-p) (server-start))))

;; Scala
(add-hook 'scala-mode-hook
	  (lambda()
	    ;; (local-set-key (kbd "C-c C-c") 'sbt-run-previous-command)))
	    (define-key scala-mode-map
	      (kbd "C-c C-c") (lambda () (interactive) (sbt-run-previous-command)))
	    (define-key scala-mode-map
	      (kbd "C-c C-t") (lambda () (interactive) (sbt-command "test")))
	    (define-key scala-mode-map
	      (kbd "C-c C-r") (lambda () (interactive) (sbt-command "run")))))

;; Valgrind
;; (defun valgrind ()
;;   (interactive)
;;   (compilation-minor-mode)
;;   (define-key compilation-minor-mode-map (kbd "") 'comint-send-input)
;;   (define-key compilation-minor-mode-map (kbd "S-") 'compile-goto-error))
