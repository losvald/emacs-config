(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Startup
(if (version< emacs-version "24")
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-screen t)
)

;; Exit

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; Frame/Window configuration

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

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

;; Display column numbers
(setq column-number-mode t)

;; Display in frame title:
;;   <buffer_name> : ( <full_file_path> | \[<buffer_size>\] <process_status> )
(setq frame-title-format
      '("%b : "
	(:eval (if buffer-file-name
		   (file-truename buffer-file-name)
		 "[%I] %s"))))

;; Printing to PDF

(defun harden-newlines ()
  (interactive)
  "Make all the newlines in the buffer hard."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (backward-char)
      (put-text-property (point) (1+ (point)) 'hard t)
      (forward-char))))

(defun spool-buffer-given-name (name)
  (load "ps-print")
  (let ((tmp ps-left-header))
    (unwind-protect
        (progn
          (setq ps-left-header
                (list (lambda () name) 'ps-header-dirpart))
          (ps-spool-buffer-with-faces))
      (setf ps-left-header tmp))))

(defun print-to-pdf ()
  "Print the current file to /tmp/print.pdf"
  (interactive)
  (let ((wbuf (generate-new-buffer "*Wrapped*"))
        (sbuf (current-buffer)))
    (jit-lock-fontify-now)
    (save-current-buffer
      (set-buffer wbuf)
      (insert-buffer sbuf)
      (longlines-mode t)
      (harden-newlines)
      (spool-buffer-given-name (buffer-name sbuf))
      (kill-buffer wbuf)
      (switch-to-buffer "*PostScript*")
      (write-file "/tmp/print.ps")
      (kill-buffer (current-buffer)))
    (call-process "ps2pdf14" nil nil nil
                  "/tmp/print.ps" "/tmp/print.pdf")
    (delete-file "/tmp/print.ps")
    (message "PDF saved to /tmp/print.pdf")))

;; Key bindings
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'x-clipboard-yank)
(global-set-key (kbd "C-c 3") 'fix-horizontal-size)
(global-set-key (kbd "C-c 4")
		(lambda ()
		  (interactive)
		  (fix-horizontal-size 164)))
(global-set-key (kbd "C-c =")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height 128)))
(global-set-key (kbd "C-c 0")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height 104)))
(global-set-key (kbd "C-c -")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height
				      (- (face-attribute 'default :height) 8))))
(global-set-key (kbd "C-c +")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height
				      (+ (face-attribute 'default :height) 8))))
