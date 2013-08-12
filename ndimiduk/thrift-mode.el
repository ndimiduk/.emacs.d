;;; thrift-mode.el --- Major mode for editing Thrift IDL files

;; Copyright is ambiguous!

;; Author: unknown
;; Keywords: thrift
;; Version: 0.0.1
;; URL: alas, I am a vagabond.

;; This file is not part of Emacs

;; Insert customary lack of warranty here.

;;; Commentary:

;; This is not owned by, nor maintained by me (Nick Dimiduk). I lifted it
;; unscrupulously from https://gist.github.com/2470924 which is itself a fork
;; of https://gist.github.com/770490 which in turn came from
;; https://github.com/kragen/stevej-emacs/blob/master/thrift-mode/thrift.el
;; and indeed is a fork of
;; https://github.com/stevej/emacs/blob/master/vendor/thrift-mode/thrift-mode.el.
;; Header and comments are all my own. So is the internet...
;;

;;; Installation:

;; Put this file somewhere on your `load-path'. You'll want to add
;; something along these lines to your init.el:
;;
;;    (require 'thrift-mode)
;;    (add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))
;;

;;; Known Bugs:

;; Bugs are unsupported. Reports to the contrary which are unaccompanied by a
;; patch will be customarily ignored.

;;; Code:

(require 'font-lock)

(defvar thrift-mode-hook nil)
(add-to-list 'auto-mode-alist '("\\.thrift\\'" . thrift-mode))

(defvar thrift-indent-level 2
  "Defines 2 spaces for thrift indentation.")

;; syntax coloring
(defconst thrift-font-lock-keywords
  (list
   '("#.*$" . font-lock-comment-face)  ;; perl style comments
   '("\\<\\(include\\|struct\\|union\\|namespace\\|exception\\|typedef\\|php_namespace\\|const\\|enum\\|service\\|extends\\|void\\|async\\|throws\\|optional\\|required\\)\\>" . font-lock-keyword-face)  ;; keywords
   '("\\<\\(bool\\|byte\\|i16\\|i32\\|i64\\|double\\|string\\|binary\\|map\\|list\\|set\\)\\>" . font-lock-type-face)  ;; built-in types
   '("\\<\\([A-Z]\\w*\\)\\>" . font-lock-type-face)   ;; typenames (unions & structs)
   '("\\<\\([0-9]+\\)\\>" . font-lock-variable-name-face)   ;; ordinals
   '("\\<\\(\\w+\\)\\s-*(" (1 font-lock-function-name-face))  ;; functions
   )
  "Thrift Keywords")

;; indentation
(defun thrift-indent-line ()
  "Indent current line as Thrift code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(}\\|throws\\)")
          (if (looking-at "^[ \t]*}")
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (- (current-indentation) thrift-indent-level)))
                (if (< cur-indent 0)
                    (setq cur-indent 0)))
            (progn
              (save-excursion
                (forward-line -1)
                (if (looking-at "^[ \t]*[\\.<>[:word:]]+[ \t]+[\\.<>[:word:]]+[ \t]*(")
                    (setq cur-indent (+ (current-indentation) thrift-indent-level))
                  (setq cur-indent (current-indentation))))))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^.*{[^}]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) thrift-indent-level))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*throws")
                  (progn
                    (setq cur-indent (- (current-indentation) thrift-indent-level))
                    (if (< cur-indent 0)
                        (setq cur-indent 0))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              (if (looking-at "^[ \t]*[\\.<>[:word:]]+[ \t]+[\\.<>[:word:]]+[ \t]*([^)]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) thrift-indent-level))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
		(cond 
		  ((looking-at "^[ \t]*\\/\\*.*\\*\\/")
                  (progn
                    (setq cur-indent (current-indentation))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
		  ((looking-at "^[ \t]*\\/\\*")
                  (progn
                    (setq cur-indent (+ (current-indentation) 1))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
		  ((looking-at "^[ \t]*\\*\\/")
                  (progn
                    (setq cur-indent (- (current-indentation) 1))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil))))
              ))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;; C/C++ comments; also allowing underscore in words
(defvar thrift-mode-syntax-table
  (let ((thrift-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" thrift-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" thrift-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" thrift-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" thrift-mode-syntax-table)
    thrift-mode-syntax-table)
  "Syntax table for thrift-mode")

(defun thrift-mode ()
  "Mode for editing Thrift files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table thrift-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(thrift-font-lock-keywords))
  (setq major-mode 'thrift-mode)
  (setq mode-name "Thrift")
  (run-hooks 'thrift-mode-hook)
  (set (make-local-variable 'indent-line-function) 'thrift-indent-line)
  )
(provide 'thrift-mode)
