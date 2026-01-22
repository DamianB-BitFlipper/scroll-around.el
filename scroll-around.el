;;; scroll-around.el --- Scroll view by paragraph/line while keeping cursor position -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Damian B
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, scrolling
;; URL: https://github.com/DamianB-BitFlipper/scroll-around.el

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

;; This package provides commands to scroll the view by paragraph or line
;; while keeping the cursor at the same screen position.  Unlike
;; `forward-paragraph' and `backward-paragraph' which move the cursor,
;; these commands move the view instead.
;;
;; Usage:
;;   (require 'scroll-around)
;;   (scroll-around-mode 1)
;;
;; This enables the following keybindings:
;;   C-S-<down>  - scroll-forward-paragraph
;;   C-S-<up>    - scroll-backward-paragraph
;;   S-<down>    - scroll-forward-line
;;   S-<up>      - scroll-backward-line

;;; Code:

(defgroup scroll-around nil
  "Scroll view while keeping cursor position."
  :group 'scrolling
  :prefix "scroll-around-")

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

(defun scroll-forward-line ()
  "Scroll view forward by one line, keeping cursor at same screen position."
  (interactive)
  (let ((screen-line (count-screen-lines (window-start) (point))))
    (scroll-up 1)
    (goto-char (window-start))
    (vertical-motion screen-line)))

(defun scroll-backward-line ()
  "Scroll view backward by one line, keeping cursor at same screen position."
  (interactive)
  (let ((screen-line (count-screen-lines (window-start) (point))))
    (scroll-down 1)
    (goto-char (window-start))
    (vertical-motion screen-line)))

(defvar scroll-around-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-S-<down>") #'scroll-forward-paragraph)
    (define-key map (kbd "C-S-<up>") #'scroll-backward-paragraph)
    (define-key map (kbd "S-<down>") #'scroll-forward-line)
    (define-key map (kbd "S-<up>") #'scroll-backward-line)
    map)
  "Keymap for `scroll-around-mode'.")

;;;###autoload
(define-minor-mode scroll-around-mode
  "Global minor mode for scrolling by paragraph or line.

When enabled, provides keybindings to scroll the view while
keeping the cursor at the same screen position.

\\{scroll-around-mode-map}"
  :global t
  :lighter " Scroll"
  :keymap scroll-around-mode-map
  :group 'scroll-around)

(provide 'scroll-around)
;;; scroll-around.el ends here
