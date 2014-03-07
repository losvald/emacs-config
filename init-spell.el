(add-hook 'text-mode-hook 'flyspell-mode)
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
		c-mode-common-hook
                shell-mode-hook
                python-mode-hook
                ruby-mode-hook
                perl-mode-hook
                LaTeX-mode-hook
                javascript-mode-hook
                yaml-mode
                php-mode-hook
		html-mode
                css-mode-hook
                crontab-mode-hook))
  (add-hook hook (lambda()
		   (flyspell-prog-mode)
		   (when (fboundp 'ac-start)
		     (ac-flyspell-workaround))
		   (setq flyspell-issue-message-flag nil))))

;; Associate text-mode with common filenames (and thus start flyspell)
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("INSTALL" . text-mode) auto-mode-alist))
