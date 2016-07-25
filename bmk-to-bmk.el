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
  :group 'bookmark)

(defvar bmk-to-bmk-stack nil)

;; Create bookmark record for bookmark-menu-list from current default
(defun bmk-to-bmk-record-function ()
  `((filename . ,bookmark-default-file)
    (handler . bmk-to-bmk-handler)))

(defun bmk-to-bmk-handler (bmk-record)
  (push bookmark-default-file bmk-to-bmk-stack)
  (bookmark-save)
  (setq bookmark-default-file (bookmark-get-filename bmk-record))
  (setq boomkark-alist nil)
  (let (bookmarks-already-loaded nil)
    (bookmark-maybe-load-default-file))
  (bookmark-bmenu-list))

;; Backtrack
(defun bmk-to-bmk-back ()
  (interactive)
  (when (> (length bmk-to-bmk-stack) 0)
    (bookmark-save)
    (setq bookmark-default-file (pop bmk-to-bmk-stack))
    (setq bookmark-alist nil)
    (let (bookmarks-already-loaded nil)
      (bookmark-maybe-load-default-file))
    (bookmark-bmenu-list)))

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
  :keymap '(((kbd "b") . bmk-to-bmk-back))
  :lighter "B2B"
  (setq-local bookmark-make-record-function 'bmk-to-bmk-record-function))

;;;###autoload
(add-hook 'bookmark-bmenu-mode-hook #'bmk-to-bmk)

(provide 'bmk-to-bmk)

;;; bmk-to-bmk-utils.el ends here