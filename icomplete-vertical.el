;;; icomplete-vertical.el --- Display icomplete candidates vertically  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience, completion
;; Version: 0.3
;; Homepage: https://github.com/oantolin/icomplete-vertical
;; Package-Requires: ((emacs "26.1"))

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
;; completion candidates vertically.  You could get a vertical display
;; without this package, by using (setq icomplete-separator "\n"), but
;; that has two problems which `icomplete-vertical-mode' solves:
;;
;; 1. Usually the minibuffer prompt and the text you type scroll off
;;    to the left!  This conceals the minibuffer prompt, and worse,
;;    the text you enter.
;;
;; 2. The first candidate appears on the same line as the one you are
;;    typing in.  This makes it harder to visually scan the candidates
;;    as the first one starts in a different column from the others.
;;
;; For users that prefer the traditional horizontal mode for Icomplete
;; but want to define some commands that use vertical completion, this
;; package provides the `icomplete-vertical-do' macro.  For example to
;; define a command to yank from the kill-ring using completion:
;;
;; (defun insert-kill-ring-item ()
;;   "Insert item from kill-ring, selected with completion."
;;   (interactive)
;;   (icomplete-vertical-do (:separator "\n··········\n" :height 20)
;;     (insert (completing-read "Yank: " kill-ring nil t))))
;;
;; Both the :separator and :height are optional and default to
;; icomplete-vertical-separator and to
;; icomplete-vertical-prospects-height, respectively.
;; If you omit both parts you still need to include the empty
;; parenthesis: (icomplete-vertical-do () ...)!.

;;; Code:

(require 'icomplete)
(require 'cl-lib)

(defgroup icomplete-vertical nil
  "Display icomplete candidates vertically."
  :group 'icomplete)

(defface icomplete-vertical-group-title
  '((t :inherit shadow :slant italic))
  "Face used for the title text of the candidate group headlines."
  :group 'icomplete-vertical)

(defface icomplete-vertical-group-separator
  '((t :inherit shadow :strike-through t))
  "Face used for the separator lines of the candidate groups."
  :group 'icomplete-vertical)

(defface icomplete-vertical-separator
  '((t :inherit shadow))
  "Face for icomplete-vertical separator.
This face is only applied if the separator string does not
already have face properties."
  :group 'icomplete-vertical)

(defcustom icomplete-vertical-prospects-height 10
  "Minibuffer height when using icomplete vertically."
  :type 'integer
  :group 'icomplete-vertical)

(defun icomplete-vertical-set-separator (separator)
  "Set the vertical candidate separator to SEPARATOR."
  (set-default 'icomplete-vertical-separator separator)
  (when (bound-and-true-p icomplete-vertical-mode)
    (setq icomplete-separator separator)
    (icomplete-vertical--setup-separator)))

(defcustom icomplete-vertical-separator-alist
  `((newline     . "\n")
    (thin-line .
               ,(concat
                 (propertize "\n" 'face '(:height 1))
                 (propertize " "
                             'face '(:inherit icomplete-vertical-separator
                                     :underline t :height 1)
                             'display '(space :align-to right))
                 (propertize "\n" 'face '(:height 1))))
    (solid-line  . "\n——————————\n")
    (dashed-line . "\n----------\n")
    (dotted-line . "\n··········\n"))
  "Alist of named candidate separators for vertical icompletion.
The symbols used as keys in this alist can be used as values for
`icomplete-vertical-separator', as arguments to
`icomplete-vertical-set-separator' or as the `:separator' option
of `icomplete-vertical-do'.

If you want to add a propertized string as a named separator from
a Custom buffer, select the \"Propertized string\" type from the
list, focus its text field, and enter its value with
\\[universal-argument] \\[eval-expression] followed by your
`propertize' call."
  :type '(alist
          :key-type (symbol :tag "Name")
          :value-type
          (choice (string :tag "String")
                  (sexp :tag "Propertized string")))
  :group 'icomplete-vertical)

(declare-function widget-convert "wid-edit")

(define-widget 'icomplete-vertical-named-separator 'choice
  "A named separator for Icomplete Vertical.
The valid names are the keys of the variable
`icomplete-vertical-separator-alist'."
  :convert-widget
  (lambda (widget)
    (widget-put widget :args
     (cl-loop for (key . _) in icomplete-vertical-separator-alist
              for name = (replace-regexp-in-string "-" " "
                            (capitalize (symbol-name key)))
              collect (widget-convert `(const :tag ,name ,key))))
    widget))

(defcustom icomplete-vertical-separator 'newline
  "Candidate separator when using icomplete vertically.
The separator should contain at least one newline.

If you change the value using setq it won't take effect until the
next time you enter Icomplete Vertical mode.  Customizing makes
it take effect immediately.  To change the value from Lisp code
use `icomplete-vertical-set-separator'.

If you want to set the value to a propertized string from a
Custom buffer, select the \"Custom propertized string\" type from
the list, focus its text field, and enter its value with
\\[universal-argument] \\[eval-expression] followed by your
`propertize' call."
  :type '(choice
          (icomplete-vertical-named-separator :tag "Named separator")
          (string :tag "Custom string")
          (sexp :tag "Custom propertized string"))
  :group 'icomplete-vertical
  :initialize 'custom-initialize-default
  :set (lambda (_ separator)
         (icomplete-vertical-set-separator separator)))

(defcustom icomplete-vertical-group-format
  (concat
   #("    " 0 4 (face icomplete-vertical-group-separator))
   #(" %s " 0 4 (face icomplete-vertical-group-title))
   #(" " 0 1 (face icomplete-vertical-group-separator
              display (space :align-to right))))
  "Format string used for the group title."
  :type '(choice (const nil) string))

(defcustom icomplete-vertical-candidates-below-end nil
  "Should candidates appear directly below the end of the input?
If this variable is non-nil the candidates instead of appearing
flush-left, will appear below the end of the minibuffer input."
  :type 'boolean
  :group 'icomplete-vertical)

(defvar icomplete-vertical--saved-state nil
  "Alist of certain variables and their last known value.
Records the values when Icomplete Vertical mode is turned on.
The values are restored when Icomplete Vertical mode is turned off.")

(defmacro icomplete-vertical--save-values (saved &rest bindings)
  "Save state of variables prior to Icomplete Vertical mode activation.
Bind variables according to BINDINGS and set SAVED to an alist of
their previous values.  Each element of BINDINGS is a
list (SYMBOL VALUEFORM) which binds SYMBOL to the value of
VALUEFORM."
  `(setq
    ,saved (list ,@(cl-loop for (var _) in bindings
                            collect `(cons ',var ,var)))
    ,@(apply #'append bindings)))

;; This next function is taken from Selectrum, written by @clemera,
;; who used it to solve the same bleeding prompt problem that this
;; package suffered from.
(defun icomplete-vertical--display-string (str)
  "Return the string which STR displays as.
This replaces portions of STR that have display properties with
the string they will display as."
  (let ((len (length str)) (pos 0) chunks)
    (while (not (eq pos len))
      (let ((end (next-single-property-change pos 'display str len))
            (display (get-text-property pos 'display str)))
        (push (if (stringp display) display (substring str pos end))
              chunks)
        (setq pos end)))
    (apply #'concat (nreverse chunks))))

(defun icomplete-vertical--format-completions (completions)
  "Reformat COMPLETIONS for better aesthetics.
To be used as filter return advice for `icomplete-completions'."
  (save-match-data
    (let ((reformatted
           (cond
            ((string-match "^\\((.*)\\|\\[.*\\]\\)?{\\(\\(?:.\\|\n\\)*\\)}$"
                           completions)
             (format "%s \n%s"
                     (or (match-string 1 completions) "")
                     (let ((candidates (match-string 2 completions)))
                       (if icomplete-vertical-candidates-below-end
                           (let* ((pad (make-string (current-column) ? ))
                                  (sep (concat "\n" pad)))
                             (concat pad
                                     (replace-regexp-in-string "\n" sep
                                                               candidates)))
                         candidates))))
            ((string-match "^\\([([]\\)\n" completions)
             (concat (match-string 1 completions)
                     (substring completions 2)))
            (t completions))))
      (when (eq t (cdr (assq 'resize-mini-windows
                             icomplete-vertical--saved-state)))
        (unless (one-window-p)
          (enlarge-window (- (min icomplete-prospects-height
                                  (cl-count ?\n reformatted))
                             (1- (window-height))))))
      (icomplete-vertical--display-string reformatted))))

(defun icomplete-vertical--annotate (completions)
  "Add annotations to COMPLETIONS.
To be used as filter return advice for `icomplete--sorted-completions'."
  (let* ((metadata (completion--field-metadata (icomplete--field-beg)))
         (annotator (completion-metadata-get metadata 'annotation-function))
         (grouper (completion-metadata-get metadata 'group-function)))
    (if (not (and (or annotator grouper) (consp completions)))
        completions
      (cl-loop
       with last-title
       for idx from 1 to icomplete-vertical-prospects-height
       for candidate in completions
       for formatted-title = nil
       for transformed-candidate = candidate
       do
       (when-let (new-title (and icomplete-vertical-group-format
                                 grouper
                                 (funcall grouper candidate nil)))
         (unless (equal new-title last-title)
           (setq last-title new-title
                 formatted-title
                 (propertize
                  "\n"
                  'line-prefix
                  (format icomplete-vertical-group-format new-title))))
         (setq transformed-candidate (funcall grouper candidate 'transform)))
       (when annotator
         (when-let (annotation (funcall annotator candidate))
           (unless (text-property-not-all
                    0 (length annotation)
                    'face nil
                    annotation)
             (add-face-text-property
              0 (length annotation)
              'completions-annotations
              t
              annotation))
           (setq transformed-candidate
                 (concat transformed-candidate annotation))))
       (when formatted-title
         (setq transformed-candidate
               (concat formatted-title transformed-candidate)))
       collect transformed-candidate into annotated
       finally (setcdr (last annotated) (cdr (last completions)))
       finally return annotated))))


;; The following function is taken from minad's Vertico package
(defun icomplete-vertical--group-by (fun elems)
  "Group ELEMS by FUN."
  (when elems
    (let ((group-list) (group-hash (make-hash-table :test #'equal)))
      (while elems
        (let* ((key (funcall fun (car elems) nil))
               (group (gethash key group-hash)))
          (if group
              (setcdr group (setcdr (cdr group) elems)) ;; Append to tail of group
            (setq group (cons elems elems)) ;; (head . tail)
            (push group group-list)
            (puthash key group group-hash))
          (setq elems (cdr elems))))
      (setcdr (cdar group-list) nil) ;; Unlink last tail
      (setq group-list (nreverse group-list))
      (prog1 (caar group-list)
        (while (cdr group-list)
          (setcdr (cdar group-list) (caadr group-list)) ;; Link groups
          (setq group-list (cdr group-list)))))))

(defun icomplete-vertical--all-sorted-completions (orig &optional start end)
  "Group sorted COMPLETIONS by `group-function'.
ORIG is the original function, which takes START and END arguments."
  (unless completion-all-sorted-completions
    (funcall orig start end)
    (when-let (grouper (and completion-all-sorted-completions
                            (completion-metadata-get
                             (completion--field-metadata
                              (or start (minibuffer-prompt-end)))
                             'group-function)))
      (let* ((last (last completion-all-sorted-completions))
             (save (cdr last)))
        (setcdr last nil)
        (setq completion-all-sorted-completions
              (icomplete-vertical--group-by
               grouper completion-all-sorted-completions))
        (setcdr (last completion-all-sorted-completions) save))))
  completion-all-sorted-completions)

(defun icomplete-vertical--minibuffer-setup ()
  "Setup minibuffer for a vertical icomplete session.
Meant to be added to `icomplete-minibuffer-setup-hook'."
  (visual-line-mode -1) ; just in case
  (setq truncate-lines t)
  (when (boundp 'auto-hscroll-mode)
    (setq-local auto-hscroll-mode 'current-line))
  (unless (one-window-p)
    (enlarge-window (- icomplete-prospects-height (1- (window-height))))))

(defun icomplete-vertical--setup-separator ()
  "Put the Icomplete Vertical separator in canonical form.
If it is a key in the `icomplete-vertical-separator-alist',
replace it by the corresponding value.  Then apply the default
separator face if the separator is a faceless string."
  (when (symbolp icomplete-separator)
    (let ((lookup (assq icomplete-separator
                        icomplete-vertical-separator-alist)))
      (if lookup
          (setq icomplete-separator (cdr lookup))
        (error "Unknown named icomplete-vertical separator: %s"
               icomplete-separator))))
  (unless (or (get-text-property 0 'face icomplete-separator)
              (next-single-property-change 0 'face icomplete-separator))
    (add-face-text-property 0 (length icomplete-separator)
                            'icomplete-vertical-separator
                            nil
                            icomplete-separator)))

(defun icomplete-vertical--minibuffer-teardown ()
  "Undo minibuffer setup for a vertical icomplete session.
This is used when toggling Icomplete Vertical mode while the
minibuffer is in use."
  (setq truncate-lines nil)
  (unless (one-window-p)
    (enlarge-window (- (1- (window-height))))))

;;;###autoload
(define-minor-mode icomplete-vertical-mode
  "Display icomplete candidates vertically."
  :global t
  (if icomplete-vertical-mode
      (progn
        (icomplete-vertical--save-values
         icomplete-vertical--saved-state
         (icomplete-mode t)
         (icomplete-separator icomplete-vertical-separator)
         (icomplete-hide-common-prefix nil)
         (icomplete-show-matches-on-no-input t)
         (resize-mini-windows 'grow-only)
         (icomplete-prospects-height icomplete-vertical-prospects-height))
        (unless icomplete-mode (icomplete-mode)) ; Don't disable `fido-mode'.
        (icomplete-vertical--setup-separator)
        (advice-add 'icomplete-completions
                    :filter-return #'icomplete-vertical--format-completions)
        (advice-add #'completion-all-sorted-completions
                    :around #'icomplete-vertical--all-sorted-completions)
        (advice-add #'icomplete--sorted-completions
                    :filter-return #'icomplete-vertical--annotate)
        (add-hook 'icomplete-minibuffer-setup-hook
                  #'icomplete-vertical--minibuffer-setup
                  5)
        (when (window-minibuffer-p)
          (icomplete-vertical--minibuffer-setup)))
    (cl-loop for (variable . value) in icomplete-vertical--saved-state
             do (set variable value))
    (unless icomplete-mode (icomplete-mode -1))
    (advice-remove 'icomplete-completions
                   #'icomplete-vertical--format-completions)
    (advice-remove 'icomplete--sorted-completions
                   #'icomplete-vertical--annotate)
    (advice-remove #'completion-all-sorted-completions
                   #'icomplete-vertical--all-sorted-completions)
    (remove-hook 'icomplete-minibuffer-setup-hook
                 #'icomplete-vertical--minibuffer-setup)
    (when (window-minibuffer-p)
      (icomplete-vertical--minibuffer-teardown))))

;;;###autoload
(defun icomplete-vertical-toggle ()
  "Toggle Icomplete Vertical mode without echo area message."
  (interactive)
  (icomplete-vertical-mode 'toggle))

(defmacro icomplete-vertical-do (params &rest body)
  "Evaluate BODY with vertical completion configured by PARAMS.
The PARAMS argument should be an alist with allowed keys
`:separator' and `:height'.  The separator should contain a
newline and can have text properties controlling its display."
  (declare (indent 1))
  (let ((hook (make-symbol "hook"))
        (verticalp (make-symbol "verticalp")))
    (cl-flet ((config (key var)
                      (let ((val (plist-get params key)))
                        (when val `(,var ,val)))))
      `(cl-flet
           ((,hook ()
                   (when icomplete-vertical-mode
                     (setq ,@(config :separator 'icomplete-separator)
                           ,@(config :height 'icomplete-prospects-height))
                     (icomplete-vertical--setup-separator))))
         (let ((,verticalp icomplete-vertical-mode))
           (icomplete-vertical-mode -1)
           (unwind-protect
               (progn
                 (add-hook 'icomplete-vertical-mode-hook #',hook)
                 (icomplete-vertical-mode)
                 ,@body)
             (icomplete-vertical-mode -1)
             (remove-hook 'icomplete-vertical-mode-hook #',hook)
             (when ,verticalp (icomplete-vertical-mode))))))))

(provide 'icomplete-vertical)
;;; icomplete-vertical.el ends here
