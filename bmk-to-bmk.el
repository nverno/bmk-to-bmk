;;; bmk-to-bmk --- Jump between different bookmark files.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/bmk-to-bmk
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 24 July 2016

;;; Commentary:

;;  Minor mode to manage jumping b/w bookmark files, displaying bookmark
;;  menus.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

(require 'bookmark)

(defgroup bmk-to-bmk nil
  "Manage jumping between/bookmarking multiple bookmark files."
  :group 'bookmark
  :prefix "bmk-to-bmk-")

;; ------------------------------------------------------------
;;* User Variables
(defcustom bmk-to-bmk-default-directory
  (expand-file-name ".emacs.d/etc/data/bookmarks" "~")
  "Default directory to store bookmark files."
  :group 'bmk-to-bmk
  :type 'file)

(defcustom bmk-to-bmk-bookmark-regexp "^.*\\.bmk$"
  "Regexp to match bookmark entries."
  :group 'bmk-to-bmk
  :type 'regexp)
 
(defface bmk-to-bmk-bookmark-highlight
  '((((background dark)) (:background "light blue" :foreground "black"))
    (t (:background "light blue")))
  "Face to highlight bookmark entries (.bmk)."
  :group 'bmk-to-bmk)
 
;; ------------------------------------------------------------
;;* Internal
(defvar bookmark-alist)
(defvar bmk-to-bmk-stack nil)

;; Create bookmark record for bookmark-menu-list from current default
(defun bmk-to-bmk-record-function ()
  `((filename . ,bookmark-default-file)
    (handler . bmk-to-bmk-handler)))

(defun bmk-to-bmk-handler (bmk-record)
  (push bookmark-default-file bmk-to-bmk-stack)
  (when (> bookmark-alist-modification-count 0)
    (bookmark-save))
  (setq bookmark-default-file (bookmark-get-filename bmk-record))
  (setq bookmark-alist nil)
  (let (bookmarks-already-loaded)
    (bookmark-maybe-load-default-file))
  (bookmark-bmenu-list))

(defun bmk-to-bmk-make-record (filename &optional name)
  (cons (or name nil)
   `((filename . ,filename)
     (handler . bmk-to-bmk-handler))))

;; ------------------------------------------------------------
;;* User Functions

(defun bmk-to-bmk-back ()
  "Go back to last bookmark file, saving current if modified."
  (interactive)
  (when (> (length bmk-to-bmk-stack) 0)
    (when (> bookmark-alist-modification-count 0)
      (bookmark-save))
    (setq bookmark-default-file (pop bmk-to-bmk-stack))
    (setq bookmark-alist nil)
    (let (bookmarks-already-loaded)
      (bookmark-maybe-load-default-file))
    (bookmark-bmenu-list)))

(defun bmk-to-bmk-new (filename &optional current link)
  "Create new bookmark file, prompting for FILENAME. If `current-prefix-arg' is 
4 or CURRENT is non-nil, set new bookmark file as current default bookmark file.  
If `current-prefix-arg' is 16 or LINK is non-nil, create link to new 
bookmark file from current bookmark menu list."
  (interactive
   (list
    (let ((default-directory (or bmk-to-bmk-default-directory
                                 default-directory)))
      (read-file-name "Bookmark File: "))))
  (when (not (file-exists-p filename))
    (message "Creating new bookmark file at %s" filename)
    (with-temp-buffer
      (let (bookmark-alist)
        (bookmark-save nil filename))))
  (when (or link (equal current-prefix-arg '(16)))
    (let* ((name (read-from-minibuffer "Bookmark Link Name: "))
           (record (bmk-to-bmk-make-record filename)))
      (bookmark-store name (cdr record) t)))
  (when (or current (equal current-prefix-arg '(4)))
    (bmk-to-bmk-handler
     (bmk-to-bmk-make-record filename))))

;; Font-lock
(defvar-local bmk-to-bmk-highlighted nil)

(defun bmk-to-bmk-toggle-highlight ()
  "Toggle highlighting of bookmark entries in *Bookmark List* buffer."
  (interactive)
  (if bmk-to-bmk-highlighted
      (progn
        (bmk-to-bmk-add-highlight bmk-to-bmk-bookmark-regexp
                                  'bmk-to-bmk-bookmark-highlight)
        (setq bmk-to-bmk-highlighted t))
    (bmk-to-bmk-remove-highlight)
    (setq bmk-to-bmk-highlighted nil)))

(defun bmk-to-bmk-add-highlight (regexp face)
  "Highlight bookmark entries in *Bookmark List* buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put overlay 'face face))
      (goto-char (match-end 0)))))

(defun bmk-to-bmk-remove-highlight ()
  "Remove highlighting of bookmark entries."
  (remove-overlays))

;; ------------------------------------------------------------
;;* Minor Mode
;; To jump between bookmark files, and bookmark as such.

;;;###autoload
(define-minor-mode bmk-to-bmk
  "Toggle bmk-to-bmk mode.
Interactively with no arguments, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix 
argument disables it.  From lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When bmk-to-bmk mode is enabled, bookmark menus can be both bookmarked
and jumped between."
  :keymap '(((kbd "b") . bmk-to-bmk-back)
            ((kbd "n") . bmk-to-bmk-new)
            ((kbd "f") . bmk-to-bmk-toggle-highlight))
  :lighter "B2B"
  (setq-local bookmark-make-record-function 'bmk-to-bmk-record-function))

;;;###autoload
(add-hook 'bookmark-bmenu-mode-hook #'bmk-to-bmk)

(provide 'bmk-to-bmk)

;;; bmk-to-bmk-utils.el ends here
