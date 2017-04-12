;;; pyutils.el --- Somme helper function for python programming

;; Copyright (C) 2016 Djangoliv'

;; Author: Djangoliv <djangoliv@mailoo.com>
;; URL:  https://github.com/djangoliv/pyutils.el
;; Version: 0.1
;; Keywords: python.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use pyutils.el, make sure that this file is in Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/")
;;
;; Then require org-week-tracker
;; (require 'pyutils)

;;; Code:

(defvar pyutils-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p c") 'pyutils-execute-buffer)
    (define-key map (kbd "C-c p w") 'pyutils-where)
    (define-key map (kbd "C-c p l") 'pyutils-lines-to-list)
    (define-key map (kbd "C-c p d") 'pyutils-generate-docstring)
    (define-key map (kbd "C-<kp-subtract>") 'hide-body-recenter)
    (define-key map (kbd "C-<kp-add>") 'outline-show-all)
    (define-key map (kbd "C-S-<kp-subtract>") 'hide-subtree)
    (define-key map (kbd "C-S-<kp-add>") 'show-subtree)
    map)
  "Keymap for Eimp mode.")

;;;###autoload
(define-minor-mode pyutils-mode
  "Somme extensions to python mode."
  :lighter " pyutils"
  :keymap pyutils-minor-mode-map
  ;; prefix
  (setq pyutils-keyboard-prefix (kbd "C-c p"))
  (add-hook 'python-mode-hook 'pyutils-fold-hook)
  ;; Use archive mode to open Python eggs
  (add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))
  ;; Use python-mode to open .pylintrc
  (add-to-list 'auto-mode-alist '("\\pylintrc\\'" . python-mode)))

;; asynchronous python buffer execution
(defun pyutils-execute-buffer (arg)
  (interactive "P")
  (let ((options ""))
    (if (not (equal arg nil))
        (setq options (read-from-minibuffer "Args: ")))
    (with-output-to-temp-buffer "*pyOutput*"
      (async-shell-command (concat "python " (buffer-file-name) " " options ";echo '\n >> job finish with status:' $?") "*pyOutput*"))))

;; strip string
(defun trim-string (string)
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))
;; show indentation levels
(defun pyutils-where ()
  ;; show all indent lines (for, if, while, def...) (path)
  (interactive)
  (save-excursion
    (back-to-indentation)
    (while (and (= 0 (current-column)) (not (bobp)))
      (forward-line -1)
      (back-to-indentation))
    (let ((theLine (thing-at-point 'line)) (col (current-column)) (way ""))
      (save-excursion
        ;; stop on class definition
        (while (and (not (or (string-match "^class" theLine)(string-match "^def" theLine))) (> col 0))
          (setq theLine (thing-at-point 'line))
          (back-to-indentation)
          ;; if non comment
          (if (not (string-match "^[ \t\n]*#" theLine))
              ;; if not empty line and indentation level
              (if (and (not (= (length (trim-string theLine)) 0)) (> col (current-column)))
                  (progn
                    (setq way (concat theLine way))
                    (setq col (current-column))
                    ;; else (or elif) case => show if
                    (if (or (string-match ".*elif .*:*" theLine) (string-match ".*else.*:*" theLine))
                        (progn
                          (forward-line -1)
                          (setq theLine (thing-at-point 'line))
                          (back-to-indentation)
                          (setq col (current-column))
                          ;; while line is empty or comment or different indent and if not in line
                          (while (or (string-match "^[ \t\n]*#" theLine) (= (length (trim-string theLine)) 0)
                                     (or (< col (current-column))
                                         (and (not (string-match ".*[ \t]if .*:*" theLine)) (not (string-match ".*for .*:*" theLine)) (not (string-match ".*try.*:*" theLine)) )))
                            (forward-line -1)
                            (back-to-indentation)
                            (setq theLine (thing-at-point 'line))
                            ;; show if elif or except
                            (if (and (or (string-match ".*elif *:*" theLine) (string-match ".*except*:*" theLine)) (not (string-match "^[ \t\n]*#" theLine)))
                                (setq way (concat theLine way ))))
                          (setq way (concat theLine way )))))))
          (forward-line -1)))
      ;; if way is empty
      (if (and (or (looking-at "[[:space:]]*$")(= 0 (current-column))) (> col 0))
          (progn
            (save-excursion
              (while (and (not (or (string-match "^class" theLine)(not (bobp)))))
                (forward-line -1)
                (setq theLine (thing-at-point 'line)))
              (setq way theLine))))
      (message way))))

;; change lines to list TODO voir make list
(defun pyutils-lines-to-list (point mark)
  (interactive "r")
  """  word1
       word2          => ['word1', 'word2', 'word3']
       word3  """
  (let ((text (buffer-substring point mark)))
    (delete-region point mark)
    (insert "[")
    (insert (mapconcat (lambda (line) (format "'%s'" line))
                       (split-string text "\s*\n\s*")
                       ", "))
    (insert "]")))

;;;; documentation generation
(defun get-function-definition(sentence)
  (if (string-match "def.*(.*):" sentence)
      (match-string 0 sentence))
  )
(defun get-parameters(sentence)
  (let ((y (get-function-definition sentence)))
    (if y
        (if (string-match "(.*)" y)
            (match-string 0 y)))))
(autoload 'thing-at-point "thingatpt" nil t) ;; build-in librairie
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string
     "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))
(defun pyutils-generate-docstring()
  (interactive)
  (forward-line 1)
  (insert "    \'\'\'\n")
  (let ((params (split-string (get-parameters (thing-at-point 'sentence)) "[?\,?\(?\)?\ ]")))
    (while params
      (if (/= (length (chomp (car params))) 0)
          (progn
            (insert "    @param ")
            (insert (chomp (car params)))
            (insert ": \n")))
      (setq params (cdr params)))
    (insert "    \'\'\'\n")))

;; FOLDING
(defun py-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))
(defun hide-body-recenter ()
  (interactive)
  (outline-hide-body)
  (recenter))
(defun pyutils-fold-hook ()
  (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(def[ \t]+\\|class[ \t]+\\)")
  (setq outline-level 'py-outline-level)
  (outline-minor-mode t))

;; Insertion du shebang python
(defun pyutils-shebang-insert ()
  (interactive)
  (insert
"#!/usr/bin/env python
# -*- coding: iso-8859-15 -*
")
  (end-of-line))

;; Hightlight debug annotation
(make-face 'pyutil-hi-face)
(set-face-attribute 'pyutil-hi-face nil :background "cadetblue")
(defvar pyutil-debug-keywords '("import ipdb" "import pdb" "set_trace()" "FIXME" "TODO") "debug word list")
(defun pyutils-annotate-debug ()
  (interactive)
  (let ((case-fold-search nil))
    (mapcar (lambda (keyword)
              (highlight-regexp keyword 'pyutil-hi-face))
            pyutil-debug-keywords)))
(defun pyutils-deannotate-debug ()
  (interactive)
  (let ((case-fold-search nil))
    (mapcar (lambda (keyword)
              (unhighlight-regexp keyword))
            pyutil-debug-keywords)))

;; TODO: generate-docstring => indentation, placement, except self
;;       refactoring
;;       doc
;;       package
;;       include in my conf
;;       test
;;       just one hook
;;       plusieurs shebang classiques

;; add the mode to the `features' list
(provide 'pyutils)

;; Local Variables:
;; coding: utf-8
;; End:

;;; py-utils.el ends here

