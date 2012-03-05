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

;; friendly colors
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; Emacs launched through Spotlight isn't run via a shell and thus
;; isn't in an environment where ~/.bash* have run.
(setenv "PATH"
        (concat (concat (getenv "HOME") "/bin" ":")
                "/usr/local/bin" ":"
                "/usr/local/sbin" ":"
                "/usr/local/share/python" ":"
                (getenv "PATH")))

;; Add brew paths to exec-path so things like aspell and markdown can
;; be found. ~/bin too, for good measure.
(push "/usr/local/share/python" exec-path)
(push "/usr/local/sbin" exec-path)
(push "/usr/local/bin" exec-path)
(push (concat (getenv "HOME") "/bin") exec-path)

;; misc
(put 'upcase-region 'disabled nil)
(push '("Rakefile" . ruby-mode) auto-mode-alist)
(push '("\\.md" . markdown-mode) auto-mode-alist)

;; clojure-mode tweaks
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; configure gist
;; don't forget `git config --global github.user`, &c.
(require 'gist)
(setq gist-view-gist t)
(setq gist-use-curl t)
(push '(slime-repl-mode . "clj") gist-supported-modes-alist)

;;
;; [2011-11-04] jacked-up jack-in
;;
;; clojure-mode jack-in appears to not correctly read slime and/or
;; slime-repl from `lein jack-in` with swank-clojure-1.4.0-SNAPSHOT.
;; Make due by installing slime and slime-repl from marmalade and
;; requiring them here.
;;
;;(require 'slime)
;;(require 'slime-repl)

