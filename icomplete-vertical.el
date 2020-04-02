;;; -*- lexical-binding: t; -*-

(defcustom icomplete-vertical-prospects-height 10
  "Minibuffer height when using icomplete vertically."
  :type 'integer
  :group 'icomplete-vertical)

(defvar icomplete-vertical-old-separator nil
  "A place to store the value of `icomplete-separator' when
icomplete-vertical-mode was turned off. This value is restored
when icomplete-vertical-mode is turned off.")

(defvar icomplete-vertical-old-hide-common nil
  "A place to store the value of `icomplete-hide-common-prefix'
when icomplete-vertical-mode was turned off. This value is
restored when icomplete-vertical-mode is turned off.")

(defun icomplete-vertical-format-completions (completions)
  "Reformat completions for better aesthetics. Meant to be used
as filter return advice for `icomplete-completions'."
  (save-match-data
    (if (string-match "^\\((.*)\\|\\[.+\\]\\)?{\\(\\(?:.\\|\n\\)+\\)}"
                      completions)
        (format "%s {\n%s}"
                (or (match-string 1 completions) "")
                (match-string 2 completions))
      completions)))

(defun icomplete-vertical-minibuffer-setup ()
  "Setup minibuffer for a vertical icomplete session. Meant to be
added to `icomplete-minibuffer-setup-hook'."
  (visual-line-mode -1) ;just in case
  (setq truncate-lines t)
  (enlarge-window (1- icomplete-vertical-prospects-height)))

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
