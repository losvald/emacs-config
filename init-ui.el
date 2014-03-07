(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Startup
(setq inhibit-splash-screen t)
(add-hook 'after-init-hook 'toggle-frame-maximized t) ; vertically maximize

; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(defun fix-frame-horizontal-size (width)
  "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (if window-system
      (set-frame-width (selected-frame) (or width 80))
    (error "Cannot resize frame horizontally: is a text terminal")))

(defun fix-window-horizontal-size (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))

(defun fix-horizontal-size (width)
  "Set the window's or frame's width to 80 (or prefix arg WIDTH)."
  (interactive "P")
  (condition-case nil
      (fix-window-horizontal-size width)
    (error
     (condition-case nil
	  (fix-frame-horizontal-size width)
       (error
	(error "Cannot resize window or frame horizontally"))))))

;; Key bindings
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'x-clipboard-yank)
(global-set-key (kbd "C-c 3") 'fix-horizontal-size)
