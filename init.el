(load "~/.emacs.d/init-ui.el")	   ; init-ui must have no dependencies

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://stable.melpa.org/packages/"))
  (package-initialize))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(load "~/.emacs.d/init-local")
(load "~/.emacs.d/init-spell.el")
(load "~/.emacs.d/init-yas") ;; should be loaded before ac
(load "~/.emacs.d/init-ac")
; use a separate file for Customize
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file) (write-region "" nil custom-file))
(load custom-file)

(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Key bindings
(global-set-key (kbd "C-c ~") 'reload-dotemacs)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'next-buffer)

(defun my/bindkey-recompile ()
  "Bind C-c C-c to `recompile'."
  (local-set-key (kbd "C-c C-c") 'recompile))

;; Core settings
; enable auto revert (reload)
(global-auto-revert-mode 1)
; always end a file with a newline

(setq require-final-newline 'query)
(defun my/delete-trailing-whitespace ()
  "Calls delete-trailing-whitespace unless in certain major modes."
  (unless (string= (with-current-buffer (current-buffer) major-mode)
		   "markdown-mode")
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my/delete-trailing-whitespace)

; jump to the beginning of matched string in I-search when C-v is pressed
(define-key isearch-mode-map (kbd "C-v") 'my-isearch-forward-to-beginning)
(defun my-isearch-forward-to-beginning ()
      "Do a forward search and jump to the beginning of the search-term."
      (interactive)
      (isearch-repeat 'forward)
      (goto-char isearch-other-end))

(require 'highlight)
(require 'unbound)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook
	  (lambda()
	    (setq flycheck-navigation-minimum-level (quote error))))

;; C/C++
; Set Google Style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook
	  (lambda()
	    (my/bindkey-recompile)
	    (c-set-offset 'inextern-lang 0)  ; don't indent "extern" blocks
	    ))
(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq flycheck-gcc-language-standard "c++11")))
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

;; Go
(eval-after-load 'go-mode
  '(progn
     (require 'go-autocomplete)
     (define-key go-mode-map (kbd "M-i") 'ac-complete-go)))
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq gofmt-command "goimports")
	    (add-hook 'before-save-hook 'gofmt-before-save)
	    (my/bindkey-recompile)
	    (let ((my/go-rel-pkg-dir
		   (file-relative-name
		    (file-name-directory (buffer-file-name))
		    (concat (file-name-as-directory
			     ;; XXX: use last component in GOPATH (assume UNIX)
			     (replace-regexp-in-string "^[^:]*:" ""
						       (getenv "GOPATH")))
			    "src"))))
	      (set (make-local-variable 'compile-command)
		   (if (string-prefix-p ".." my/go-rel-pkg-dir)
		       (format "go run '%s'" (buffer-file-name))
		     (format "go install '%s'" my/go-rel-pkg-dir))))))

;; JavaScript
(add-to-list
 'auto-mode-alist
 '("\\.js\\'" . (lambda ()
		  (require 'ac-js2)
		  (js2-mode))))
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook
 'markdown-mode-hook
 (lambda ()
   (setq ; auto-indentation uses tabs to align at tab stops but spaces otherwise
    indent-tabs-mode nil ; always use spaces to avoid surprises/non-determinism
    tab-width 4)))

;; Octave / Matlab
(setq auto-mode-alist
      (cons
       '("\\.m$" . octave-mode)
       auto-mode-alist))

;; Python

(require 'python)
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (add-hook 'python-mode-hook	 ; make python-shell work with IPython
	    (lambda ()
	      (setq
	       python-shell-prompt-regexp "In \\[[0-9]+\\]: \\|>>> "
	       python-shell-prompt-block-regexp "   \\.\\.\\.+:? "
	       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
	       python-shell-completion-setup-code
	       "from IPython.core.completerlib import module_completion"
	       python-shell-completion-string-code
	       "';'.join(get_ipython().Completer.all_completions('%s'))\n"))))

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

(add-hook
 'python-mode-hook
 (lambda ()
   (set (make-local-variable 'comment-inline-offset) 2)  ; >=2 spaces before #
   (setq
    indent-tabs-mode nil
    tab-width 4)
   (ad-activate 'python-calculate-indentation)
   (my/bindkey-recompile)
   (define-key python-mode-map (kbd "C-c C-d") 'python-shell-send-defun)
   (define-key python-mode-map (kbd "C-c C-h") 'python-shell-send-buffer)
   (define-key python-mode-map (kbd "M-n") 'flycheck-next-error)
   (define-key python-mode-map (kbd "M-p") 'flycheck-previous-error)))

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

;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook
	  (lambda ()
	    (my/bindkey-recompile)
	    (set (make-local-variable 'compile-command)
		 (if (locate-dominating-file
		      (file-name-directory (buffer-file-name)) "Cargo.toml")
		     "cargo build"
		   (format "rustc '%s'" (buffer-file-name))))))

;; Scala
(when (fboundp 'use-package)
  (use-package ensime :ensure t :pin melpa)
  (use-package sbt-mode :pin melpa)
  (use-package scala-mode :pin melpa))
(defun my/scala-mode-hook ()
  (unless (require 'sbt-mode nil 'noerror)
    (warn "SBT mode not installed"))
  (if (fboundp 'scala-mode) (scala-mode)
    (lwarn "emacs" :error "No scala-mode[2] installed")))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . my/scala-mode-hook))
(defun my/amm-mode-hook ()
  (scala-mode)
  (set (make-local-variable 'compile-command)
       (concat "amm --predef-code "
	       "'interp.colors() = ammonite.util.Colors.BlackWhite' "
	       (buffer-file-name)))
  (when (fboundp 'ensime-mode)
    (define-key ensime-mode-map (kbd "C-c C-c") nil))
  (define-key scala-mode-map (kbd "C-c C-c") 'recompile))
(add-to-list 'auto-mode-alist '("\\.sc\\'" . my/amm-mode-hook)) ; for amm
;; (eval-after-load 'scala-mode2 '(require 'sbt-mode))
(add-hook 'scala-mode-hook
	  (lambda()
	    (define-key scala-mode-map
	      (kbd "C-c C-s") (lambda ()
				(interactive)
				(sbt-run-previous-command)))
	    (define-key scala-mode-map
	      (kbd "C-c C-M-c") (lambda () (interactive) (sbt-command "clean")))
	    (define-key scala-mode-map
	      (kbd "C-c C-t") (lambda () (interactive) (sbt-command "test")))
	    (define-key scala-mode-map
	      (kbd "C-c C-r") (lambda () (interactive) (sbt-command "run")))))
(add-to-list 'display-buffer-alist	; reuse SBT compilation window
	     '("^\\*sbt\\*" display-buffer-reuse-window (reusable-frames . t)))

;; SQL
(eval-after-load "sql" '(load-library "sql-indent"))

;; Valgrind
;; (defun valgrind ()
;;   (interactive)
;;   (compilation-minor-mode)
;;   (define-key compilation-minor-mode-map (kbd "") 'comint-send-input)
;;   (define-key compilation-minor-mode-map (kbd "S-") 'compile-goto-error))
