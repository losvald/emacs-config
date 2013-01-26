;;; auto-complete-auctex  --- auto-complete for LaTeX auctex

;; Copyright (C) 2010 Guy Yeterian gyecmonet <@> free <dot> fr

;; Author: Guy Yeterian
;; Maintainer: the author
;; Created: 07 Aug 2010
;; Version: 1.0
;; version  1.8 Jun 2011
;; version  1.9 Aug 2011
;; Keywords: auctex auto-complete



;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;;  or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; ac-latex|rockymyroot|address@hidden
;; |auto-complete for LaTeX auctex
;; |$Date$|$Revision$|~/packages/ac-latex.el

;;; Installation

;; This is a very useful package to write LaTeX documents with aucTeX and auto-complete

;;  To use this package you must install aucTeX and auto-complete packages.
;;  Just add to the .emacs file:
;;     (autoload 'ac-latex-setup "auto-complete-latex" "ac and aucTeX" t)
;;     (add-hook 'LaTeX-mode-hook (lambda() (ac-latex-setup)))

;; when you type 'enu' you must see all (even yours) laTeX-environments and TeX-macros that

;;  begins with this words.

;; Because of the use of auto-complete package you must customize this for yours needs.

;;  In particular  see ac-delay,  ac-auto-show-menu and ac-quick-help-delay.


;;; Commentary:
;;  Free softwards are great and make me happy.

;; Many thanks to Richard Stallman for free softward concept and for emacs.

;;  Many thanks for the aucTeX group.
;;  Many thanks for auto-complete author Tomohiro Matsuyama.

;;; Change log:
;; $Log$

;;; Code:
(provide 'auto-complete-auctex)

(require 'tex)
(require 'latex)


(eval-when-compile
  (require 'auto-complete))
  ;;(require 'popup-pos-tip))



;; some custome and globals functions

(defface ac-latex-candidate-face
  '((t (:background "LemonChiffon1" :foreground "black")))
  "Face for LaTeX candidate."
  :group 'LaTeX)

(defface ac-latex-selection-face
  '((t (:background "goldenrod" :foreground "black")))
  "Face for the LaTeX selected candidate."
  :group 'LaTeX)


;; add LaTeX-mode to ac-modes
(setq ac-modes (append '(LaTeX-mode latex-mode) ac-modes))



(defvar candidate nil
  "Just for the Elisp compiler not complain")



(defun my-LaTeX-math-insert (s dollar)
  "Insert s with dollar if necessary"
  (when dollar (insert "$"))
  (funcall LaTeX-math-insert-function s)
  (if dollar
      (progn
        (let ((d (point)))
          (re-search-forward "[{.*}]*\}" (+ (point) 10) t)
          (insert "$")
          (goto-char d))))
  ) ;; end my-LaTeX-math-insert



;; Function to insert TeX-macro
(defun ac-symbols-insert (s)
  "Insert math symbol s with dollar if necessary"
  (let ((dollar (if (texmathp)
                    nil
                  t)))
    (my-LaTeX-math-insert s dollar)
    )) ;; ac-symbols-insert




;;; TeX-macros source and action

(defun ac-source-latex-macros ()
   (let ((comlist (if TeX-symbol-list
                     (mapcar (lambda (x)
                       (if (listp x)
                           (car x)
                         x))
                       (mapcar 'car TeX-symbol-list)))))
    (all-completions ac-prefix comlist))) ;; ac-latex-source-commandes

;; end globas functions





;;; action on macros
(defun action-auctex-macro ()
  (re-search-backward candidate)
  (delete-region (match-beginning 0) (match-end 0))
  (LaTeX-math-insert candidate nil)
  ) ;; action-auctex-commande



(ac-define-source  LaTeX-macros
  '((init . TeX-symbol-list)
    (candidates . ac-source-latex-macros)
    (document . TeXdoc)
    (cache)
    ;(depends . '(LaTeX-mode  auto-complete-mode))
    (action . action-auctex-macro)
    (requires . 2)
    (candidate-face . ac-latex-candidate-face)
    (selection-face . ac-latex-selection-face)
    (symbol . "m")));; LaTeX-macros

;; end TeX macros




;; ac-latex-symbols with LaTeX-math-mode
(defvar ac-symbols-auctex
  (mapcar 'cadr LaTeX-math-default))

(defvar ac-symbols-list
  (mapcar 'cdr LaTeX-math-default))


;; candidate
(defun ac-symbols-candidate ()
  "List of math symbol"
  (all-completions ac-prefix ac-symbols-auctex))




;; action


(defun ac-symbols-action ()
  (re-search-backward candidate)
  (delete-region (match-beginning 0) (match-end 0))
  (ac-symbols-insert candidate))


;; some documentation
(defun ac-symbols-doc (c)
  "Some help for the candidate"
  (let* ((cl (assoc c ac-symbols-list))
         (decode (if (nth 2 cl) (char-to-string (decode-char 'ucs (nth 2 cl))) 
""))
         (st (nth 1 cl))
         (hs (if (listp st)(mapconcat 'identity st " ")st))
         (help (concat hs " == " decode))
         )
    help
    )
  ) ;;  ac-symbols-doc


(defun LaTeX-math-mode-on ()
  (LaTeX-math-mode 1))


(ac-define-source LaTeX-symbols
  '((init . LaTeX-math-mode-on)
    (candidates . ac-symbols-candidate)
    (document . ac-symbols-doc)
    (action . ac-symbols-action)
    (requires . 1)
    (candidate-face . ac-latex-candidate-face)
    (selection-face . ac-latex-selection-face)
    (symbol . "s")
    )
  ) ;; LaTeX-symbols

;; ac-sources-LaTeX-symbols

;;;  LaTeX-environmants source and action

(defun ac-source-latex-environments ()
    (let ((envlist (mapcar 'car LaTeX-environment-list))
        )

    (all-completions ac-prefix envlist))
  ) ;; ac-latex-source-environments

;; action pour les environments
(defun action-auctex-environment ()
  (re-search-backward candidate)
  (delete-region (match-beginning 0) (match-end 0))
  (LaTeX-environment-menu candidate)
  ) ;; action-auctex-environment



(ac-define-source LaTeX-environments
  '((init . LaTeX-environment-list)
    (candidates . ac-source-latex-environments)
    (document .  TeXdoc)
    (cache)
    (action .  action-auctex-environment)
    (requires . 2)
    (selection-face . ac-latex-selection-face)
    (candidate-face . ac-latex-candidate-face)
    (symbol . "e")))

;;; end LaTeX environments source and action






;; some help if you work with TeXlive
;; you can change it if too long. F1 give help immediateley
;; this work with TeXlive
(setq ac-quick-help-delay 4.5)

;; work with TeXlive
(defun TeXdoc (item)
  (interactive)
  (TeX-doc item))
 ;; end of TeXdoc





;; setup  you call ac-latex-setup with LaTeX-mode-hook
;;;###autoload
(defun ac-latex-setup ()
  (interactive)
  (TeX-symbol-list)
  (LaTeX-environment-list)
  (setq ac-sources (append
                      '(ac-source-LaTeX-symbols
                        ac-source-LaTeX-macros ac-source-LaTeX-environments)
                      ac-sources))
  )
;;; auto-complete-auctex  --- auto-complete for LaTeX auctex

;; Copyright (C) 2010 Guy Yeterian gyecmonet <@> free <dot> fr

;; Author: Guy Yeterian
;; Maintainer: the author
;; Created: 07 Aug 2010
;; Version: 1.0
;; version  1.8 Jun 2011
;; version  1.9 Aug 2011
;; Keywords: auctex auto-complete



;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; address@hidden) or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; ac-latex|rockymyroot|address@hidden
;; |auto-complete for LaTeX auctex
;; |$Date$|$Revision$|~/packages/ac-latex.el

;;; Installation

;; This is a very useful package to write LaTeX documents with aucTeX and auto-complete

;;  To use this package you must install aucTeX and auto-complete packages.
;;  Just add to the .emacs file:
;;     (autoload 'ac-latex-setup "auto-complete-latex" "ac and aucTeX" t)
;;     (add-hook 'LaTeX-mode-hook (lambda() (ac-latex-setup)))

;; when you type 'enu' you must see all (even yours) laTeX-environments and TeX-macros that

;;  begins with this words.

;; Because of the use of auto-complete package you must customize this for yours needs.

;;  In particular  see ac-delay,  ac-auto-show-menu and ac-quick-help-delay.


;;; Commentary:
;;  Free softwards are great and make me happy.

;; Many thanks to Richard Stallman for free softward concept and for emacs.

;;  Many thanks for the aucTeX group.
;;  Many thanks for auto-complete author Tomohiro Matsuyama.

;;; Change log:
;; $Log$

;;; Code:
(provide 'auto-complete-auctex)

(require 'tex)
(require 'latex)


(eval-when-compile
  (require 'auto-complete))
  ;;(require 'popup-pos-tip))



;; some custome and globals functions

(defface ac-latex-candidate-face
  '((t (:background "LemonChiffon1" :foreground "black")))
  "Face for LaTeX candidate."
  :group 'LaTeX)

(defface ac-latex-selection-face
  '((t (:background "goldenrod" :foreground "black")))
  "Face for the LaTeX selected candidate."
  :group 'LaTeX)


;; add LaTeX-mode to ac-modes
(setq ac-modes (append '(LaTeX-mode latex-mode) ac-modes))



(defvar candidate nil
  "Just for the Elisp compiler not complain")



(defun my-LaTeX-math-insert (s dollar)
  "Insert s with dollar if necessary"
  (when dollar (insert "$"))
  (funcall LaTeX-math-insert-function s)
  (if dollar
      (progn
        (let ((d (point)))
          (re-search-forward "[{.*}]*\}" (+ (point) 10) t)
          (insert "$")
          (goto-char d))))
  ) ;; end my-LaTeX-math-insert



;; Function to insert TeX-macro
(defun ac-symbols-insert (s)
  "Insert math symbol s with dollar if necessary"
  (let ((dollar (if (texmathp)
                    nil
                  t)))
    (my-LaTeX-math-insert s dollar)
    )) ;; ac-symbols-insert




;;; TeX-macros source and action

(defun ac-source-latex-macros ()
   (let ((comlist (if TeX-symbol-list
                     (mapcar (lambda (x)
                       (if (listp x)
                           (car x)
                         x))
                       (mapcar 'car TeX-symbol-list)))))
    (all-completions ac-prefix comlist))) ;; ac-latex-source-commandes

;; end globas functions





;;; action on macros
(defun action-auctex-macro ()
  (re-search-backward candidate)
  (delete-region (match-beginning 0) (match-end 0))
  (LaTeX-math-insert candidate nil)
  ) ;; action-auctex-commande



(ac-define-source  LaTeX-macros
  '((init . TeX-symbol-list)
    (candidates . ac-source-latex-macros)
    (document . TeXdoc)
    (cache)
    ;(depends . '(LaTeX-mode  auto-complete-mode))
    (action . action-auctex-macro)
    (requires . 2)
    (candidate-face . ac-latex-candidate-face)
    (selection-face . ac-latex-selection-face)
    (symbol . "m")));; LaTeX-macros

;; end TeX macros




;; ac-latex-symbols with LaTeX-math-mode
(defvar ac-symbols-auctex
  (mapcar 'cadr LaTeX-math-default))

(defvar ac-symbols-list
  (mapcar 'cdr LaTeX-math-default))


;; candidate
(defun ac-symbols-candidate ()
  "List of math symbol"
  (all-completions ac-prefix ac-symbols-auctex))




;; action


(defun ac-symbols-action ()
  (re-search-backward candidate)
  (delete-region (match-beginning 0) (match-end 0))
  (ac-symbols-insert candidate))


;; some documentation
(defun ac-symbols-doc (c)
  "Some help for the candidate"
  (let* ((cl (assoc c ac-symbols-list))
         (decode (if (nth 2 cl) (char-to-string (decode-char 'ucs (nth 2 cl))) 
""))
         (st (nth 1 cl))
         (hs (if (listp st)(mapconcat 'identity st " ")st))
         (help (concat hs " == " decode))
         )
    help
    )
  ) ;;  ac-symbols-doc


(defun LaTeX-math-mode-on ()
  (LaTeX-math-mode 1))


(ac-define-source LaTeX-symbols
  '((init . LaTeX-math-mode-on)
    (candidates . ac-symbols-candidate)
    (document . ac-symbols-doc)
    (action . ac-symbols-action)
    (requires . 1)
    (candidate-face . ac-latex-candidate-face)
    (selection-face . ac-latex-selection-face)
    (symbol . "s")
    )
  ) ;; LaTeX-symbols

;; ac-sources-LaTeX-symbols

;;;  LaTeX-environmants source and action

(defun ac-source-latex-environments ()
    (let ((envlist (mapcar 'car LaTeX-environment-list))
        )

    (all-completions ac-prefix envlist))
  ) ;; ac-latex-source-environments

;; action pour les environments
(defun action-auctex-environment ()
  (re-search-backward candidate)
  (delete-region (match-beginning 0) (match-end 0))
  (LaTeX-environment-menu candidate)
  ) ;; action-auctex-environment



(ac-define-source LaTeX-environments
  '((init . LaTeX-environment-list)
    (candidates . ac-source-latex-environments)
    (document .  TeXdoc)
    (cache)
    (action .  action-auctex-environment)
    (requires . 2)
    (selection-face . ac-latex-selection-face)
    (candidate-face . ac-latex-candidate-face)
    (symbol . "e")))

;;; end LaTeX environments source and action






;; some help if you work with TeXlive
;; you can change it if too long. F1 give help immediateley
;; this work with TeXlive
(setq ac-quick-help-delay 4.5)

;; work with TeXlive
(defun TeXdoc (item)
  (interactive)
  (TeX-doc item))
 ;; end of TeXdoc





;; setup  you call ac-latex-setup with LaTeX-mode-hook
;;;###autoload
(defun ac-latex-setup ()
  (interactive)
  (TeX-symbol-list)
  (LaTeX-environment-list)
  (setq ac-sources (append
                      '(ac-source-LaTeX-symbols
                        ac-source-LaTeX-macros ac-source-LaTeX-environments)
                      ac-sources))
  )

(provide 'auto-complete-auctex)
;;;  end of auto-complete-package
