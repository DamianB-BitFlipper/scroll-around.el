;;; scroll-paragraph.el --- Scroll view by paragraph while keeping cursor position -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Damian B
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, scrolling
;; URL: https://github.com/damianb/scroll-paragraph.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides commands to scroll the view by one paragraph
;; while keeping the cursor at the same screen position.  Unlike
;; `forward-paragraph' and `backward-paragraph' which move the cursor,
;; these commands move the view instead.
;;
;; Usage:
;;   (require 'scroll-paragraph)
;;   (global-set-key (kbd "C-S-<down>") #'scroll-forward-paragraph)
;;   (global-set-key (kbd "C-S-<up>") #'scroll-backward-paragraph)

;;; Code:

(defun scroll-forward-paragraph ()
  "Scroll view forward by one paragraph, keeping cursor at same screen position."
  (interactive)
  (let* ((screen-line (count-screen-lines (window-start) (point)))
         (lines (save-excursion
                  (let ((start (point)))
                    (forward-paragraph)
                    (count-screen-lines start (point))))))
    (scroll-up lines)
    (goto-char (window-start))
    (vertical-motion screen-line)))

(defun scroll-backward-paragraph ()
  "Scroll view backward by one paragraph, keeping cursor at same screen position."
  (interactive)
  (let* ((screen-line (count-screen-lines (window-start) (point)))
         (lines (save-excursion
                  (let ((start (point)))
                    (backward-paragraph)
                    (count-screen-lines (point) start)))))
    (scroll-down lines)
    (goto-char (window-start))
    (vertical-motion screen-line)))

(provide 'scroll-paragraph)
;;; scroll-paragraph.el ends here
