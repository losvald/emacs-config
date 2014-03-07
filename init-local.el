(setq x-select-enable-clipboard t)

(require 'python)

; Python
(add-hook 'python-mode-hook
    (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)))
