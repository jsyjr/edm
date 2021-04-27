;;; edm.el --- An emacs display manager -*- lexical-binding: t -*-

;; Author: Alex Griffin <a@ajgrf.com>
;; URL: https://github.com/ajgrf/edwina
;; Version: 0.3.0-pre
;; Package-Requires: ((emacs "25"))
;; Keywords: convenience

;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;;
;;;
;;; This file is NOT part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Edm is a dynamic window manager for Emacs. It automatically arranges your
;; Emacs panes (called "windows" in Emacs parlance) into predefined layouts,
;; dwm-style.

;;; Code:

(require 'seq)

(defgroup edm nil
  "A dynamic window manager for Emacs."
  :group 'convenience
  :prefix "edm-")

(defcustom edm-keymap-prefix (kbd "C-c C-w")
  "Prefix key for keybindings."
  :type 'string
  :group 'edm)

(defcustom edm-mode-line-format "%s "
  "String used for displaying the current layout in mode line."
  :type 'string
  :group 'edm)

(defcustom edm-nmaster 1
  "The number of windows to put in the Edm master area."
  :type 'integer
  :group 'edm)

(defcustom edm-mfact 0.55
  "The size of the master area in proportion to the stack area."
  :type 'float
  :group 'edm)

(defcustom edm-narrow-threshold 132
  "Put master area on top if the frame is narrower than this."
  :type 'integer
  :group 'edm)

(defvar edm-layout 'edm-tall-layout
  "The current Edm layout.
A layout is a function that takes a list of panes, and arranges them into
a window configuration.")

(defvar edm--window-fields
  '(buffer start hscroll vscroll point prev-buffers)
  "List of window fields to save and restore.")

(defvar edm--window-params
  '(delete-window quit-restore)
  "List of window parameters to save and restore.")

(defun edm-pane (window)
  "Create pane from WINDOW.
A pane is Edm's internal window abstraction, an association list containing
a buffer and other information."
  (let ((pane '()))
    (dolist (field edm--window-fields)
      (let* ((getter (intern (concat "window-" (symbol-name field))))
             (value (funcall getter window)))
        (push (cons field value) pane)))
    (dolist (param edm--window-params)
      (let ((value (window-parameter window param)))
        (push (cons param value) pane)))
    pane))

(defun edm-restore-pane (pane)
  "Restore PANE in the selected window."
  (dolist (field edm--window-fields)
    (let ((setter (intern (concat "set-window-" (symbol-name field))))
          (value  (alist-get field pane)))
      (funcall setter nil value)))
  (dolist (param edm--window-params)
    (set-window-parameter nil param (alist-get param pane)))
  (unless (window-parameter nil 'delete-window)
    (set-window-parameter nil 'delete-window #'edm-delete-window)))

(defun edm--window-list (&optional frame)
  "Return a list of windows on FRAME in layout order."
  (window-list frame nil (frame-first-window frame)))

(defun edm-pane-list (&optional frame)
  "Return the current list of panes on FRAME in layout order."
  (mapcar #'edm-pane (edm--window-list frame)))

(defmacro edm--respective-window (window &rest body)
  "Execute Edm manipulations in BODY and return the respective WINDOW."
  (declare (indent 1))
  `(let* ((window ,window)
          (windows (edm--window-list))
          (index (seq-position windows window)))
     ,@body
     (nth index (edm--window-list))))

(defun edm-arrange (&optional panes)
  "Arrange PANES according to Edm's current layout."
  (interactive)
  (let* ((panes (or panes (edm-pane-list))))
    (select-window
     (edm--respective-window (selected-window)
       (delete-other-windows)
       (funcall edm-layout panes)))))

(defun edm--display-buffer (display-buffer &rest args)
  "Apply DISPLAY-BUFFER to ARGS and arrange windows.
Meant to be used as advice :around `display-buffer'."
  (edm--respective-window (apply display-buffer args)
    (edm-arrange)))

(defun edm-stack-layout (panes)
  "Edm layout that stacks PANES evenly on top of each other."
  (let ((split-height (ceiling (/ (window-height)
                                  (length panes)))))
    (edm-restore-pane (car panes))
    (dolist (pane (cdr panes))
      (select-window
       (split-window nil split-height 'below))
      (edm-restore-pane pane))))

(defun edm--mastered (side layout)
  "Add a master area to LAYOUT.
SIDE is passed to `split-window' to position the stack area."
  (lambda (panes)
    (let ((master (seq-take panes edm-nmaster))
          (stack  (seq-drop panes edm-nmaster))
          (msize  (ceiling (* edm-mfact
                              (if (memq side '(left right t))
                                  (frame-width)
                                (frame-height))))))
      (cond ((and master stack)
             (split-window nil msize side)
             (edm-stack-layout master)
             (select-window (next-window))
             (funcall layout stack))
            (master (edm-stack-layout master))
            (stack  (funcall layout stack))))))

(defun edm-tall-layout (panes)
  "Edm layout with master and stack areas for PANES."
  (let* ((side (if (< (frame-width) edm-narrow-threshold) 'below 'right))
         (layout (edm--mastered side #'edm-stack-layout)))
    (funcall layout panes)))

(defun edm-layout-name (layout)
  "Return the user-facing name of LAYOUT."
  (capitalize
   (replace-regexp-in-string "\\(edm-\\|-layout\\)" ""
                             (symbol-name layout))))

(defun edm-mode-line-indicator ()
  "Return a string to display in the mode line."
  (format edm-mode-line-format
          (edm-layout-name edm-layout)))

(defun edm-select-next-window ()
  "Move cursor to the next window in cyclic order."
  (interactive)
  (select-window (next-window)))

(defun edm-select-previous-window ()
  "Move cursor to the previous window in cyclic order."
  (interactive)
  (select-window (previous-window)))

(defun edm-swap-next-window ()
  "Swap the selected window with the next window."
  (interactive)
  (let ((cur  (edm-pane (selected-window)))
        (next (edm-pane (next-window))))
    (edm-restore-pane next)
    (edm-select-next-window)
    (edm-restore-pane cur)))

(defun edm-swap-previous-window ()
  "Swap the selected window with the previous window."
  (interactive)
  (let ((cur  (edm-pane (selected-window)))
        (prev (edm-pane (previous-window))))
    (edm-restore-pane prev)
    (edm-select-previous-window)
    (edm-restore-pane cur)))

(defun edm-dec-mfact ()
  "Decrease the size of the master area."
  (interactive)
  (setq edm-mfact (max (- edm-mfact 0.05)
                         0.05))
  (edm-arrange))

(defun edm-inc-mfact ()
  "Increase the size of the master area."
  (interactive)
  (setq edm-mfact (min (+ edm-mfact 0.05)
                         0.95))
  (edm-arrange))

(defun edm-dec-nmaster ()
  "Decrease the number of windows in the master area."
  (interactive)
  (setq edm-nmaster (max (- edm-nmaster 1)
                            0))
  (edm-arrange))

(defun edm-inc-nmaster ()
  "Increase the number of windows in the master area."
  (interactive)
  (setq edm-nmaster (+ edm-nmaster 1))
  (edm-arrange))

(defun edm-clone-window ()
  "Clone selected window."
  (interactive)
  (split-window-below)
  (edm-arrange))

(defun edm-delete-window (&optional window)
  "Delete WINDOW."
  (interactive)
  (let ((ignore-window-parameters t))
    (delete-window window))
  (when (or (null window)
            (and (boundp 'edm-mode) edm-mode))
    (edm-arrange)))

(defun edm-zoom ()
  "Zoom/cycle the selected window to/from master area."
  (interactive)
  (if (eq (selected-window) (frame-first-window))
      (edm-swap-next-window)
    (let ((pane (edm-pane (selected-window))))
      (edm-delete-window)
      (edm-arrange (cons pane (edm-pane-list))))))

(defvar edm-mode-map
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "r") 'edm-arrange)
    (define-key prefix-map (kbd "C-r") 'edm-arrange)
    (define-key prefix-map (kbd "n") 'edm-select-next-window)
    (define-key prefix-map (kbd "C-n") 'edm-select-next-window)
    (define-key prefix-map (kbd "SPC") 'edm-select-next-window)
    (define-key prefix-map (kbd "p") 'edm-select-previous-window)
    (define-key prefix-map (kbd "C-p") 'edm-select-previous-window)
    (define-key prefix-map (kbd "N") 'edm-swap-next-window)
    (define-key prefix-map (kbd "C-S-n") 'edm-swap-next-window)
    (define-key prefix-map (kbd "P") 'edm-swap-previous-window)
    (define-key prefix-map (kbd "C-S-p") 'edm-swap-previous-window)
    (define-key prefix-map (kbd "%") 'edm-dec-mfact)
    (define-key prefix-map (kbd "{") 'edm-dec-mfact)
    (define-key prefix-map (kbd "[") 'edm-dec-mfact)
    (define-key prefix-map (kbd "^") 'edm-inc-mfact)
    (define-key prefix-map (kbd "}") 'edm-inc-mfact)
    (define-key prefix-map (kbd "]") 'edm-inc-mfact)
    (define-key prefix-map (kbd "d") 'edm-dec-nmaster)
    (define-key prefix-map (kbd "C-d") 'edm-dec-nmaster)
    (define-key prefix-map (kbd "i") 'edm-inc-nmaster)
    (define-key prefix-map (kbd "k") 'edm-delete-window)
    (define-key prefix-map (kbd "C-k") 'edm-delete-window)
    (define-key prefix-map (kbd "RET") 'edm-zoom)
    (define-key prefix-map (kbd "<return>") 'edm-zoom)
    (define-key prefix-map (kbd "c") 'edm-clone-window)
    (define-key prefix-map (kbd "C-c") 'edm-clone-window)
    (define-key map edm-keymap-prefix prefix-map)
    map)
  "Keymap for command `edm-mode'.")

(defvar edm-mode-map-alist
  `((edm-mode . ,edm-mode-map))
  "Add to `emulation-mode-map-alists' to give bindings higher precedence.")

(defvar edm-dwm-key-alist
  '(("r" edm-arrange)
    ("j" edm-select-next-window)
    ("k" edm-select-previous-window)
    ("S-j" edm-swap-next-window)
    ("J" edm-swap-next-window)
    ("S-k" edm-swap-previous-window)
    ("K" edm-swap-previous-window)
    ("h" edm-dec-mfact)
    ("l" edm-inc-mfact)
    ("d" edm-dec-nmaster)
    ("i" edm-inc-nmaster)
    ("S-c" edm-delete-window)
    ("C" edm-delete-window)

    ("RET" edm-zoom t)
    ("return" edm-zoom t)
    ("S-RET" edm-clone-window t)
    ("S-return" edm-clone-window t))

  "A list of keys to bind with a prefix. Used in
  `edm-setup-dwm-keys'")

(defun edm-setup-dwm-keys (&optional modifier)
  "Set up dwm-like keybindings. MODIFIER is the mod-key to use,
and must be a either \'super or \'hyper. With no argument,
use meta."
  (let ((mod-prefix
	 (cond
	  ((equal 'super modifier)
	   "s-")
	  ((equal 'hyper modifier)
	   "H-")
	  (t "M-"))))
    (dolist (key-and-function edm-dwm-key-alist)
      (define-key edm-mode-map
	(if (cddr key-and-function)
	    (kbd (format "<%s%s>"
			 mod-prefix
			 (car key-and-function)))
	  (kbd (format "%s%s"
		       mod-prefix
		       (car key-and-function))))
	(cadr key-and-function)))))

(defun edm--init ()
  "Initialize command `edm-mode'."
  (add-to-list 'emulation-mode-map-alists
               'edm-mode-map-alist)
  (advice-add #'display-buffer :around #'edm--display-buffer)
  (unless (assoc 'edm-mode mode-line-misc-info)
    (push '(edm-mode (:eval (edm-mode-line-indicator)))
          (cdr (last mode-line-misc-info))))
  (edm-arrange))

(defun edm--clean-up ()
  "Clean up when disabling command `edm-mode'."
  (advice-remove #'display-buffer #'edm--display-buffer))

;;;###autoload
(define-minor-mode edm-mode
  "Toggle Edm mode on or off.
With a prefix argument ARG, enable Edm mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Edm mode is a global minor mode that provides dwm-like dynamic
window management for Emacs windows."
  :global t
  (if edm-mode
      (edm--init)
    (edm--clean-up)))

(provide 'edm)
;;; edm.el ends here
