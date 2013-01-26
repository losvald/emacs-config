;; AUCTeX
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; auto-complete-latex setup
(autoload 'ac-latex-setup "auto-complete-latex" "ac and aucTeX" t)
(add-hook 'LaTeX-mode-hook (lambda() (ac-latex-setup)))
