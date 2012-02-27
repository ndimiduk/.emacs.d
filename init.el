;;
;; emacs starter kit v2, http://technomancy.us/153
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell
                                  starter-kit-bindings starter-kit-js
                                  starter-kit-ruby scpaste
                                  clojure-mode clojure-test-mode
                                  markdown-mode yaml-mode paredit
                                  magit color-theme color-theme-solarized))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)
(put 'upcase-region 'disabled nil)

;; add brew exec's to PATH
;; (setenv "PATH"
;;         (concat (concat (getenv "HOME") "/bin" ":")
;;                 (getenv "PATH")
;;                 "/usr/local/bin" ":"
;;                 "usr/local/sbin" ":"))

;; (push (concat (getenv "HOME") "/bin") exec-path)
;; (push "/usr/local/bin" exec-path)
;; (push "/usr/local/sbin" exec-path)

;; above path hacking for brew hasn't worked. take the easy way out
;; and hard-code per utility.
(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq-default markdown-command "/usr/local/bin/markdown")

(setq browse-url-browser-function 'browse-default-macosx-browser)
