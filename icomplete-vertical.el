;;; icomplete-vertical.el --- Global minor mode to display icomplete candidates vertically  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple-minded global minor mode to comfortably use
;; icomplete to display completion candidates vertically.  A simple
;; (setq icomplete-separator "\n") gets you 95% of the way there, this
;; small package just adds a few visual tweaks on top of that.

;;; Code:
(require 'icomplete)

(defcustom icomplete-vertical-prospects-height 10
  "Minibuffer height when using icomplete vertically."
  :type 'integer
  :group 'icomplete)

(defvar icomplete-vertical-old-separator nil
  "Store last known value of `icomplete-separator'.
Records the value when `icomplete-vertical-mode' is turned on.
It is then restored when icomplete-vertical-mode is turned off.")

(defvar icomplete-vertical-old-hide-common nil
  "Store last known value of `icomplete-hide-common-prefix'.
Records the value when `icomplete-vertical-mode' is turned on.
It is then restored when icomplete-vertical-mode is turned off.")

(defun icomplete-vertical-format-completions (completions)
  "Reformat COMPLETIONS for better aesthetics.
To be used as filter return advice for `icomplete-completions'."
  (save-match-data
    (if (string-match "^\\((.*)\\|\\[.+\\]\\)?{\\(\\(?:.\\|\n\\)+\\)}"
                      completions)
        (format "%s {\n%s}"
                (or (match-string 1 completions) "")
                (match-string 2 completions))
      completions)))

(defun icomplete-vertical-minibuffer-setup ()
  "Setup minibuffer for a vertical icomplete session.
Meant to be added to `icomplete-minibuffer-setup-hook'."
  (visual-line-mode -1) ;just in case
  (setq truncate-lines t)
  (enlarge-window (1- icomplete-vertical-prospects-height)))

;;;###autoload
(define-minor-mode icomplete-vertical-mode
  "Display icomplete candidates vertically."
  :global t
  (if icomplete-vertical-mode
      (progn
        (setq icomplete-vertical-old-separator icomplete-separator
              icomplete-separator "\n"
              icomplete-vertical-old-hide-common icomplete-hide-common-prefix
              icomplete-hide-common-prefix nil)
        (advice-add 'icomplete-completions
                    :filter-return #'icomplete-vertical-format-completions)
        (add-hook 'icomplete-minibuffer-setup-hook
                  #'icomplete-vertical-minibuffer-setup
                  5))
    (setq icomplete-separator icomplete-vertical-old-separator
          icomplete-hide-common-prefix icomplete-vertical-old-hide-common)
    (advice-remove 'icomplete-completions
                   #'icomplete-vertical-format-completions)
    (remove-hook 'icomplete-minibuffer-setup-hook
                 #'icomplete-vertical-minibuffer-setup)))

(provide 'icomplete-vertical)
;;; icomplete-vertical.el ends here

