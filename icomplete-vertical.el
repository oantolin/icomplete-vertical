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

;; This package defines a global minor mode to display Icomplete
;; completion candidates vertically.  It solves certain problems that a
;; mere (setq icomplete-separator "\n") could never overcome.
;;
;; Specifically, simply setting the separator to a newline presents two
;; issues:
;;
;; 1 Usually the minibuffer prompt and the text you type scroll off to
;;   the left!  This conceals the minibuffer prompt.
;; 2 The first candidate appears on the same line as the one you are
;;   typing in.  This clutters the view and can cause confusion.
;;
;; Both of these are addressed by enabling `icomplete-vertical-mode':
;;
;; + The minibuffer prompt remains visible.
;; + The matching item appears on a line below the minibuffer input,
;;   while being vertically aligned to the list of candidates.

;;; Code:
(require 'icomplete)
(require 'cl-lib)

(defcustom icomplete-vertical-prospects-height 10
  "Minibuffer height when using icomplete vertically."
  :type 'integer
  :group 'icomplete)

(defvar icomplete-vertical-saved-state nil
  "Alist of certain variables and their last known value.
Records the values when `icomplete-vertical-mode' is turned on.
The values are restored when icomplete-vertical-mode is turned off.")

(defmacro icomplete-vertical-save-values (saved &rest bindings)
  "Save state of variables prior to `icomplete-vertical-mode' activation.
Bind variables according to BINDINGS and set SAVED to an alist of
their previous values.  Each element of BINDINGS is a
list (SYMBOL VALUEFORM) which binds SYMBOL to the value of
VALUEFORM."
  `(setq
    ,saved (list ,@(cl-loop for (var _) in bindings
                            collect `(cons ',var ,var)))
    ,@(apply #'append bindings)))

(defun icomplete-vertical-format-completions (completions)
  "Reformat COMPLETIONS for better aesthetics.
To be used as filter return advice for `icomplete-completions'."
  (save-match-data
    (if (string-match "^\\((.*)\\|\\[.+\\]\\)?{\\(\\(?:.\\|\n\\)+\\)}"
                      completions)
        (format "%s \n%s"
                (or (match-string 1 completions) "")
                (match-string 2 completions))
      completions)))

(defun icomplete-vertical-minibuffer-setup ()
  "Setup minibuffer for a vertical icomplete session.
Meant to be added to `icomplete-minibuffer-setup-hook'."
  (visual-line-mode -1) ;just in case
  (setq truncate-lines t)
  (enlarge-window icomplete-vertical-prospects-height))

;;;###autoload
(define-minor-mode icomplete-vertical-mode
  "Display icomplete candidates vertically."
  :global t
  (if icomplete-vertical-mode
      (progn
        (icomplete-vertical-save-values
         icomplete-vertical-saved-state
         (icomplete-separator "\n")
         (icomplete-hide-common-prefix nil)
         (resize-mini-windows 'grow-only)
         (icomplete-prospects-height icomplete-vertical-prospects-height))
        (advice-add 'icomplete-completions
                    :filter-return #'icomplete-vertical-format-completions)
        (add-hook 'icomplete-minibuffer-setup-hook
                  #'icomplete-vertical-minibuffer-setup
                  5))
    (cl-loop for (variable . value) in icomplete-vertical-saved-state
             do (set variable value))
    (advice-remove 'icomplete-completions
                   #'icomplete-vertical-format-completions)
    (remove-hook 'icomplete-minibuffer-setup-hook
                 #'icomplete-vertical-minibuffer-setup)))

(provide 'icomplete-vertical)
;;; icomplete-vertical.el ends here
