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

;; Hide toolbar
(tool-bar-mode -1)

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

;; Use image+ mode if installed
(eval-after-load 'image '(require 'image+ nil 'noerror))

;; TODO: not personally tested, but supposedly a good thing
;; Fix scrolling when vc-annotate
;; (defadvice vc-annotate (around vc-annotate-and-scroll)
;;   "scroll buffer to view current line in the annotated buffer"
;;   (let ((pos (count-lines (point-min) (point))))
;;     ad-do-it
;;     (let ((orig-window (selected-window))
;;           (window (other-window-for-scrolling)))
;;       (select-window window)
;;       (goto-line pos)
;;       (select-window orig-window))))
;; (ad-activate 'vc-annotate)

;; RE builder
(require 're-builder)
(setq reb-re-syntax 'string)
(define-key reb-mode-map (kbd "C-c g")
  (lambda () (interactive) (reb-change-syntax 'string)))
(define-key reb-mode-map (kbd "C-c \\")
  (lambda () (interactive) (reb-change-syntax 'read)))

(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
    (message "Not in a re-builder buffer!")))
(define-key reb-mode-map "\C-c\M-%" 'reb-query-replace-this-regxp)

;; Global key bindings
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'x-clipboard-yank)
(global-set-key (kbd "s-o") 'other-frame)
(global-set-key (kbd "C-c 3") 'fix-horizontal-size)
(global-set-key (kbd "C-c 4")
		(lambda ()
		  (interactive)
		  (fix-horizontal-size 164)))
(global-set-key (kbd "s-=")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height 128)))
(global-set-key (kbd "s-0")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height 104)))
(global-set-key (kbd "s--")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height
				      (- (face-attribute 'default :height) 8))))
(global-set-key (kbd "s-+")
		(lambda ()
		  (interactive)
		  (set-face-attribute 'default nil :height
				      (+ (face-attribute 'default :height) 8))))
