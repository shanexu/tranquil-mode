;;; tranquil.el --- 
;; 
;; Filename: tranquil.el
;; Description: 
;; Author: Shane Xu <xusheng0711@gmail.com>
;; Maintainer: 
;; Created: Fri Apr 12 15:32:58 2013 (+0800)
;; Version: 0.1
;; Keywords: tranquil
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cc-mode)


(defvar tranquil-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tq\\'" . tranquil-mode))
(add-to-list 'auto-mode-alist '("\\xnomad\\'" . tranquil-mode))

(defvar tq-possibly-braceless-keyword-regexp
      (concat "\\_<" (regexp-opt
                      (split-string "if then else while until async whenFinished lock collect" " ") t) "\\_>"))

(defvar tq-keywords (split-string  "if then else unless while until import async wait whenFinished lock and or" " "))
(defvar tq-keywords-regexp (regexp-opt tq-keywords 'words))

(defvar tq-keyword-literal (split-string "super self yes no nil valid" " "))
(defvar tq-keyword-literal-regexp (regexp-opt tq-keyword-literal 'words))

(defvar tq-class-def-regexp-1 "^[@#]\\([A-Z][A-Za-z0-9_]*\\)\\s-*\\(<\\s-*\\([A-Z][A-Za-z0-9_]*\\)\\)?")

(defvar tq-class-def-regexp-3 "^[@#]\\([A-Z][A-Za-z0-9_]*\\)\\s-*\\(<\\s-*\\([A-Z][A-Za-z0-9_]*\\)\\)")

(defvar tq-method-regexp-1 "[+-]\\s-*\\([_a-zA-Z][_0-9a-zA-Z]*\\)\\(\\s-*:\\s-*[_a-zA-Z][_0-9a-zA-Z]*\\)?\\(\\s-+\\([_a-zA-Z][_0-9a-zA-Z]*\\)\\(\\s-*:\\s-*[_a-zA-Z][_0-9a-zA-Z]*\\)\\)*[\s\t\n\r]*[{`]")

(defvar tq-method-regexp-4 "[+-]\\s-*\\([_a-zA-Z][_0-9a-zA-Z]*\\)\\(\\s-*:\\s-*[_a-zA-Z][_0-9a-zA-Z]*\\)?\\(\\s-+\\([_a-zA-Z][_0-9a-zA-Z]*\\)\\(\\s-*:\\s-*[_a-zA-Z][_0-9a-zA-Z]*\\)\\)+[\s\t\n\r]*[{`]")

(defvar tq-instance-variable-regexp "\\(@\\|@@\\)\\(\\w\\|_\\)+")

(defvar tq-sharpbang-regexp "^#!.*$")

(defvar tq-symbol-regexp "#[a-zA-Z][0-9a-zA-Z]*\\|#\"[^\\]\"\\|#'[^']*'")

(defvar tq-font-lock-keywords
      `(
        (,tq-keywords-regexp . font-lock-keyword-face)
        (,tq-keyword-literal-regexp . font-lock-builtin-face)
        (,tq-class-def-regexp-1 1 font-lock-type-face)
        (,tq-class-def-regexp-3 3 font-lock-type-face)
        (,tq-method-regexp-1 1 font-lock-function-name-face)
        (,tq-method-regexp-4 4 font-lock-function-name-face)
        (,tq-symbol-regexp . font-lock-reference-face)
        (,tq-instance-variable-regexp . font-lock-variable-name-face)
        (,tq-sharpbang-regexp . font-lock-comment-face)
        ))


;; syntax table
(defvar tranquil-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" syntax-table)
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?\` "\"" syntax-table)
    (modify-syntax-entry ?\\ "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?$ "." syntax-table)
    (modify-syntax-entry ?_ "_" syntax-table)
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    (modify-syntax-entry ?& "." syntax-table)
    (modify-syntax-entry ?| "." syntax-table)
    (modify-syntax-entry ?% "." syntax-table)
    (modify-syntax-entry ?= "." syntax-table)
    (modify-syntax-entry ?/ "." syntax-table)
    (modify-syntax-entry ?+ "." syntax-table)
    (modify-syntax-entry ?* "." syntax-table)
    (modify-syntax-entry ?- "." syntax-table)
    (modify-syntax-entry ?\( "()" syntax-table)
    (modify-syntax-entry ?\) ")(" syntax-table)
    (modify-syntax-entry ?\{ "(}" syntax-table)
    (modify-syntax-entry ?\} "){" syntax-table)
    (modify-syntax-entry ?\[ "(]" syntax-table)
    (modify-syntax-entry ?\] ")[" syntax-table)

    syntax-table)
  "Syntax table for `tranquil-mode'.")

(defun tq-continued-expression-p ()
  )

(defun tq-re-search-backward-inner (regexp &optional bound count)
  (while (> count 0)
    (re-search-backward regexp bound)
    (when (and (> (point) (point-min))
               (save-excursion (backward-char) (looking-at "\\\\")))
      (forward-char))
    (setq parse (syntax-ppss))
    (cond ((nth 4 parse)
           (re-search-backward "\\\\"))
          (t
           (setq count (1- count)))))
  (point))


(defun tq-re-search-backward (regexp &optional bound noerror count)
  (tq-re-search-forward regexp bound noerror (if count (- count) -1)))

(defun tq-re-search-forward-inner (regexp &optional bound count)

  (while (> count 0)
    (re-search-forward regexp bound)
    (setq parse (syntax-ppss))
    (cond ((nth 7 parse)
           (forward-line))
          ((nth 4 parse)
           (re-search-forward "\\n"))
          (t
           (setq count (1- count)))))
  (point))


(defun tq-re-search-forward (regexp &optional bound noerror count)
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'tq-re-search-backward-inner)
               ((> count 0) #'tq-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun tq-ctrl-statement-indentation ()
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (progn
                   (tq-re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at (concat "\\_<" (regexp-opt '("then" "else" "async" "whenFinished") t) "\\_>")))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) default-tab-width)))))

(defun tq-proper-indentation (parse-status)
  "return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status) 0)
          ((nth 8 parse-status) 0)
          ((tq-ctrl-statement-indentation))
          ((nth 1 parse-status)
           (beginning-of-line)
           (let ((same-indent-p (looking-at "^\\s-*[]})]+\\s-*"))
                 (continued-expr-p (tq-continued-expression-p)))
             (goto-char (nth 1 parse-status))
             (cond (same-indent-p
                    (current-indentation))
                   (continued-expr-p
                    (+ (current-indentation) (* 2 default-tab-width))))
             (if same-indent-p
                 (current-indentation)
               (+ (current-indentation) default-tab-width))))
          ((nth 0 parse-status) (cond ((eq (nth 0 parse-status) 0) 0))))))

(defun tranquil-test-syntax-ppss ()
  (interactive)
  (save-excursion
    (syntax-ppss (point))))

(defun tranquil-indent-line ()
  "indent current line as tranquil code."
  (interactive)
  (save-restriction
    (widen)
    (let ((parse-status
           (save-excursion (syntax-ppss (point-at-bol))))
          (offset (- (current-column) (current-indentation))))
      (indent-line-to
       (tq-proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

(define-derived-mode tranquil-mode fundamental-mode
  "tranquil mode"
  "Major mode for editing tranquil"
  :syntax-table tranquil-syntax-table

  (set (make-local-variable 'comment-start) "\\")
  (set (make-local-variable 'font-lock-defaults) '(tq-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'tranquil-indent-line)  
  (setq mode-name "tranquil"))

(defalias 'tq-mode 'tranquil-mode)

(provide 'tranquil-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tranquil.el ends here
