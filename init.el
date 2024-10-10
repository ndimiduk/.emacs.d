;;; init.el --- This is my Emacs configurations. There are many like it, but this one is mine.
;;; Commentary:
;; Something witty --- I promise you.
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 98)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(straight-use-package-by-default t)
 '(straight-vc-git-default-clone-depth 1)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(when (fboundp 'straight-use-package)
  ;; function should be defined here because it was just loaded
  (straight-use-package 'use-package))

;; initialize host-specific configs, a la
;; https://nicolas.petton.fr/blog/per-computer-emacs-settings.html
(defvar init-hostname (substring (shell-command-to-string "hostname") 0 -1))
(defvar init-host-dir (expand-file-name "~/.emacs.d/hosts/"))
(add-to-list 'load-path init-host-dir)
(let ((init-host-feature (intern (downcase (concat "init-" init-hostname)))))
  (require init-host-feature nil 'noerror))

;; enable services provided by core Emacs without a package wrapper. These are probably configured
;; via customize, above.
(add-hook 'text-mode-hook (lambda () (auto-fill-mode t)))
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode t)))
(add-hook 'conf-mode-hook (lambda () (auto-fill-mode t)))

;(use-package bash-completion
;  :config (add-hook 'shell-dynamic-complete-functions
;                    'bash-completion-dynamic-complete))

(use-package bats-mode)

(use-package cider)

(use-package clojure-mode)

(use-package color-theme-sanityinc-solarized
  :custom
  (custom-safe-themes
   '(;; color-theme-sanityinc-solarized-dark
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     ;; color-theme-sanityinc-solarized-light
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     default))
  :config
  (when (fboundp 'color-theme-sanityinc-solarized-light)
    (color-theme-sanityinc-solarized-light)))

(use-package company
  :hook (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-<" . company-select-first)
        ("M->" . company-select-last)
        ("M-/" . company-complete)))

(use-package company-lsp
  :commands company-lsp)

(use-package compile
  :config
  (add-to-list 'compilation-error-regexp-alist-alist '(yetus-report "^|.*|[[:blank:]]+\\(/[A-Za-z0-9._/-]+\\)[[:blank:]]+|$" 1))
  (add-to-list 'compilation-error-regexp-alist 'yetus-report)
  (add-to-list 'compilation-error-regexp-alist-alist '(yetus-results "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):.*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'yetus-results))

(use-package dockerfile-mode)

(use-package exec-path-from-shell
  ;; MacOS only, work-around for Emacs launced via GUI, which is started from a minimal
  ;; environment
  :if (memq window-system '(mac ns))
  :config
  (when (fboundp 'exec-path-from-shell-initialize)
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :custom
  (global-flycheck-mode t))

(use-package flyspell
  :config
  (setq ispell-list-command "--list")
  :hook ((prog-mode . flyspell-prog-mode)
         (conf-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package groovy-mode)

(use-package hcl-mode)

(use-package hl-line
  :config (global-hl-line-mode t))

(use-package ispell
  :custom
  (ispell-program-name "aspell"))

(use-package lsp-mode
  :bind (("C-q" . lsp-ui-doc-toggle)
         ("C-." . lsp-ui-peek-find-references))
  :demand
  :custom
  (lsp-enable-snippet nil)
  (lsp-yaml-schema-store-local-db "~/.emacs.d/var/lsp/lsp-yaml-schemas.json")
  (lsp-yaml-schemas '((kubernetes . ["base/*.yaml"
                                     "overlays/**/*.yaml"])
                      (http://json\.schemastore\.org/kustomization . ["Kustomization.yaml" "kustomization.yaml"])
                      (http://json\.schemastore\.org/github-workflow\.json . [".github/workflows/*.yml"
                                                                              ".github/workflows/*.yaml"])
                      (http://json\.schemastore\.org/github-action\.json . [".github/actions/*.yml"
                                                                            ".github/actions/*.yaml"])
                      (kubernetes . ["base/*.yaml" "overlays/**/*.yaml"])))
  (lsp-groovy-server-file "~/repos/groovy-language-server/build/libs/groovy-language-server-all.jar")
  :config
  (add-to-list 'lsp-language-id-configuration '(bats-mode . "shellscript"))
;  (add-to-list 'lsp-language-id-configuration '(js-json-mode . "json"))
  :hook (bats-mode
         dockerfile-mode
         groovy-mode
         js-json-mode
         nxml-mode
         python-mode
         sh-mode
         yaml-mode)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package magit
;  :init
;  (setq vc-handled-backends nil)
  :hook
  (git-commit-setup . git-commit-turn-on-flyspell)
  :config
  ;; by default, fetch will not overwrite local tags with new definitions from upstream. Sometimes
  ;; this is a desired behavior, which is accomplished via `--force` option.
  ;; https://github.com/magit/magit/discussions/4705
  (transient-append-suffix 'magit-fetch "-t" '("-f" "Force" "--force"))
  :bind (("C-c g" . magit-status)
         ("C-x g" . magit-status)))

(use-package markdown-mode
  :hook (markdown-mode . turn-on-auto-fill))

(use-package mistty)

(use-package org
  :custom
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell .t)
                              (sqlite . t))))

;; org-roam configuration
;; Support a notes system structured as
;;   org-roam/
;;   - articles/ -- documents authored for an audience other than myself
;;   - daily/    -- date-oriented entries: journals, work logs, &c.
;;   - inbox.org -- unsorted drop-box for new comments/ideas
;;   - notes/    -- notes taken for myself
;;   - sources/  -- notes taken from source materials -- books, articles, talks, &c.
(use-package org-roam
  :after (org)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/repos/braindump/org-roam/"))
  (org-default-notes-file (concat org-roam-directory "inbox.org"))
  (org-capture-templates
   '(("i" "Inbox capture" entry (file org-default-notes-file) "* %?\n")))
  (org-roam-capture-templates
   '(("a" "Article" plain "%?"
      :if-new (file+head "article/${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("n" "Note" plain "%?"
      :if-new (file+head "note/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
      :immediate-finish t
      :unnarrowed t)
     ("s" "Source" plain "%?"
      :if-new (file+head "source/${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))
  ;; If you're using a vertical completion framework, you might want a more informative completion
  ;; interface
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)))

(use-package protobuf-mode
  :straight (protobuf-mode
              :type git
              :flavor melpa
              :files ("editors/protobuf-mode.el" "protobuf-mode-pkg.el")
              :host github
              :repo "protocolbuffers/protobuf"))

(use-package puppet-mode)

;;; Configure rustic with rust-analyzer
;(use-package f) ; missing dependency declaration for rustic
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-format-trigger 'on-save))

(use-package selectrum
  :custom (selectrum-mode t))

(use-package selectrum-prescient
  :custom
  (selectrum-prescient-mode t)
  (prescient-persist-mode t))

(use-package systemd)

(use-package yaml-mode)

(use-package yasnippet
  :commands
  (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)))

;(use-package server
  ;; explicitly configure server runtime socket
  ;; :init (setenv "XDG_RUNTIME_DIR" "${HOME}/var/run" t)
  ;; XDG_RUNTIME_DIR="${HOME}/var/run"
  ;; EDITOR="emacsclient -n -s ${XDG_RUNTIME_DIR}/emacs/server"
;  :config (unless (server-running-p) (server-start)))

;; Fake footer to clear warnings
;;(provide 'init)
;;; init.el ends here
