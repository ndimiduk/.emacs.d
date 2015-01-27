;;; ndimiduk.el -- user specific customizations

;;
;; package management
;;

(require 'package)
;; define additional package archive locations
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; this one for log4j-mode
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun ensure-packages (ps)
  "install any missing packages in ps"
  (dolist (p ps)
    (unless (package-installed-p p) (package-install p))))

;; install packages that aren't installed.
(ensure-packages
 '(
   better-defaults
   clojure-mode
   clojure-test-mode
   color-theme
   color-theme-solarized
   elisp-slime-nav
   find-file-in-project
   gist
   ibuffer-vc
   idle-highlight-mode
   ido-ubiquitous
   js2-mode
   log4j-mode
   magit
   markdown-mode
;; will not pull in latest and greatest org-mode as an older version ships with
;; emacs. likely need to install manually.
   org
   paredit
   protobuf-mode
   puppet-mode
   scpaste
   smex
   yaml-mode
))

;;
;; paths and executables
;;

;; Emacs launched through Spotlight isn't run via a shell and thus isn't in an
;; environment where ~/.bash* have run.
(setenv "PATH"
        (concat (concat (getenv "HOME") "/bin" ":")
                "/usr/local/bin" ":"
                "/usr/local/sbin" ":"
                "/usr/local/share/python" ":"
                (getenv "PATH")))

;; Add brew paths to exec-path so things like aspell and markdown can
;; be found. ~/bin too, for good measure. use gnu coreutils.
;; TODO: is this redundant to updating PATH, above?
(push "/usr/local/share/python" exec-path)
(push "/usr/local/sbin" exec-path)
(push "/usr/local/bin" exec-path)
(push (concat (getenv "HOME") "/bin") exec-path)
(push "/usr/local/opt/coreutils/libexec/gnubin" exec-path)

;;
;; ibuffer group by vc project by default
;; (https://github.com/purcell/ibuffer-vc)
;;
(defun nd-enable-ibuffer-vc-grouping ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))
(add-hook 'ibuffer-hook 'nd-enable-ibuffer-vc-grouping)

;;
;; friendly colors
;;

(push "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" custom-safe-themes) ;; solarized-light
(push "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" custom-safe-themes) ;; solarized-dark
(if (null window-system)
    (load-theme 'tsdh-dark 't)
  (load-theme 'solarized-light 't))

;;
;; magit
;;

;; magit will look for repos here and below a couple levels.
(setq magit-repo-dirs (list (getenv "HOME")))
;; don't include remote name in local tracking branches
(setq magit-default-tracking-name-function
      'magit-default-tracking-name-branch-only)
;; switching between hbase-0.94 and hbase-trunk abandons module dirs, the
;; contents of which are expensive for magit to list. Don't.
(setq magit-omit-untracked-dir-contents 't)

;;
;; gist-mode
;;

;; don't forget `git config --global github.user`, &c.
;; (require 'gist)
(setq gist-view-gist t)
(setq gist-use-curl t)
;;(push '(slime-repl-mode . "clj") gist-supported-modes-alist)

;;
;; clojure-mode
;;

;;(require 'clojure-mode)
;; (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
;; (add-hook 'slime-repl-mode-hook 'esk-turn-on-paredit)
;; (define-key clojure-mode-map (kbd "C-c v") 'slime-eval-buffer)
;; (global-set-key (kbd "C-c C-j") 'clojure-jack-in)

;; indentation rules for compojure macros
;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
;; (define-clojure-indent
;;   (defroutes 'defun)
;;   (GET 2)
;;   (POST 2)
;;   (PUT 2)
;;   (DELETE 2)
;;   (HEAD 2)
;;   (ANY 2)
;;   (context 2))

;; indentation rules for lemur jobdefs
;; (define-clojure-indent
;;   (defcluster 'defun)
;;   (defstep 'defun)
;;   (use-base 0)
;;   (catch-args 0)
;;   (add-validators 0))

(defun lein-deps ()
  (interactive)
  (let ((proj-dir (locate-dominating-file default-directory "project.clj")))
    (when (not proj-dir)
      (error "cannot find project.clj"))
    (shell-command (format "cd %s && lein do clean, deps &" proj-dir)
                   "*lein-deps*")))

(defun lein-new (path)
  (interactive "FNew project directory: ")
  (let ((parent (file-name-directory (file-truename path)))
        (target (file-name-nondirectory (file-truename path))))
    (shell-command
     (format "cd %s && lein new %s &" parent target)
     "*lein-new*")))

(defun lein-uberjar ()
  (interactive)
  (let ((proj-dir (locate-dominating-file default-directory "project.clj")))
    (when (not proj-dir)
      (error "cannot find project.clj"))
    (shell-command (format "cd %s && lein do deps, compile, uberjar &" proj-dir)
                   "*lein-uberjar*")))

;;
;; org-mode
;;

;;(require 'org)
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; (setq org-hide-leading-stars t)

;; babel-foo
;;(require 'ob-clojure)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((clojure . t)
;;    (css . t)
;;    (emacs-lisp . t)
;;    (js . t)
;;    (perl . t)
;;    (python . t)
;;    (sh . t)))

;; will not pull in latest and greatest org-mode as an older version ships with
;; emacs. likely need to install manually.
;; (eval-after-load 'org
;;   '(progn
;;      (setq org-directory "~/Dropbox/Documents/org")
;;      (org-remember-insinuate)
;;      (global-set-key (kbd "C-M-r") 'org-capture)
;;      (setq org-src-fontify-natively t)
;;      (setq org-src-tab-acts-natively t)))

;;
;; Apache, HBase project settings
;;

;; fill-width for Apache projects
(dir-locals-set-class-variables
 'apache-java-project '((java-mode . ((fill-column . 100)))))

(dir-locals-set-directory-class
 (concat (getenv "HOME") "/repos/hbase/") 'apache-java-project)

;; &c.

;; include java files in ffip
;; (push "*.java" ffip-patterns)

(provide 'ndimiduk)
