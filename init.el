;;; init.el --- This is my Emacs configurations. There are many like it, but this one is mine.
;;; Commentary:
;; Something witty --- I promise you.
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
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

;; Customize fonts when available.
;; https://typeof.net/Iosevka/
;; https://github.com/edwardtufte/et-book
;; paired fonts (fixed-pitch variable-pitch)
;;  ("Iosevka [Fixed,Term]" "Iosevka Aile")
;;  ("Iosevka [Fixed,Term] Slab" "Iosevka Etoile")
(let* ((fixed-pitch-tuple
        (cond ((x-family-fonts "Iosevka Term")
               '(:family "Iosevka Term" :width expanded :height 110))
              (t '(:family "Monospace"))))
       (fixed-pitch-serif-tuple
        (cond ((x-family-fonts "Iosevka Term Slab")
               '(:inherit fixed-pitch :family "Iosevka Term Slab"))
               (t '(:family "Monospace Serif"))))
       (variable-pitch-tuple
        (cond ((x-family-fonts "Iosevka Aile") '(:family "Iosevka Aile" :height 110))
              ;;((x-family-fonts "ETBookOT") '(:family "ETBookOT" :height 140))
              (t '(:family "Sans Serif")))))
  (custom-theme-set-faces
   'user
   `(default           ((t (,@fixed-pitch-tuple))))
   `(fixed-pitch       ((t (,@fixed-pitch-tuple))))
   `(fixed-pitch-serif ((t (,@fixed-pitch-serif-tuple))))
   `(variable-pitch    ((t (,@variable-pitch-tuple))))
   ))

;; enable services provided by core Emacs without a package wrapper. These are probably configured
;; via customize, above.
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(add-hook 'conf-mode-hook #'turn-on-auto-fill)

;; install blackout before everything else so that it can be used by everything else
(use-package blackout
  :demand t
  :functions blackout
  :config
  (with-eval-after-load 'simple (blackout 'auto-fill-mode))
  (with-eval-after-load 'autorevert (blackout 'auto-revert-mode))
  (with-eval-after-load 'face-remap (blackout 'buffer-face-mode))
  (with-eval-after-load 'org-indent (blackout 'org-indent-mode)))

;; when combined with visual-line-mode, the "word-wrap" indentation decisions are based on the
;; auto-fill-mode configuration, so it does nice things like respect bulleted lists.
(use-package adaptive-wrap
  :hook (org-mode . adaptive-wrap-prefix-mode))

(use-package bats-mode)

(use-package cider)

(use-package clojure-mode)

(use-package color-theme-sanityinc-solarized
  :demand t
  :custom
  (custom-safe-themes
   '(;; sanityinc-solarized-dark
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     ;; sanityinc-solarized-light
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     default))
  :config
  (load-theme 'sanityinc-solarized-light t))

(use-package company
  :hook (after-init . global-company-mode)
  :defines company-active-map
  :blackout company-mode
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
  :blackout flyspell-mode
  :config
  (setq ispell-list-command "--list")
  :hook ((prog-mode . flyspell-prog-mode)
         (conf-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; adds major modes for various git files (.gitignore, .gitconfig ,&c)
(use-package git-modes)

(use-package groovy-mode)

(use-package hcl-mode)

(use-package hl-line
  :config (global-hl-line-mode t))

;; aspell does not support multiple dictionaries. looking into replacement with Hunspell.
;; experiment.
;; sudo pacman -S hunspell hunspell-en_us hunspell-en_gb
;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
(use-package ispell
  :init
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US,en_GB")
  (ispell-personal-dictionary "~/.hunspell_personal")
  :config
  (when (fboundp 'ispell-set-spellchecker-params)
    (ispell-set-spellchecker-params))
  (when (fboundp 'ispell-hunspell-add-multi-dic)
    (ispell-hunspell-add-multi-dic "en_US,en_GB")))

;; (use-package ispell
;;  :custom
;;  (ispell-program-name "aspell"))

(use-package lsp-java)

(use-package lsp-mode
  :bind (("C-q" . lsp-ui-doc-toggle)
         ("C-." . lsp-ui-peek-find-references))
  :demand t
  :custom
  (lsp-enable-snippet nil)
  (lsp-yaml-schema-store-local-db "~/.emacs.d/var/lsp/lsp-yaml-schemas.json")
  (lsp-yaml-schemas
   '((kubernetes . ["base/*.yaml" "overlays/**/*.yaml"])
     (http://json\.schemastore\.org/kustomization . ["Kustomization.yaml" "kustomization.yaml"])
     (http://json\.schemastore\.org/github-workflow\.json . [".github/workflows/*.yml"
                                                             ".github/workflows/*.yaml"])
     (http://json\.schemastore\.org/github-action\.json . [".github/actions/*.yml"
                                                           ".github/actions/*.yaml"])))
  (lsp-groovy-server-file "~/repos/groovy-language-server/build/libs/groovy-language-server-all.jar")
  :defines lsp-language-id-configuration
  :config
  (add-to-list 'lsp-language-id-configuration '(bats-mode . "shellscript"))
;  (add-to-list 'lsp-language-id-configuration '(js-json-mode . "json"))
  :hook (bats-mode
         dockerfile-mode
         groovy-mode
         java-mode
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
  (transient-append-suffix 'magit-merge "-s" '("-A" "Allow unrelated histories" "--allow-unrelated-histories"))
  :bind (("C-c g" . magit-status)
         ("C-x g" . magit-status)))

(use-package markdown-mode
  :hook (markdown-mode . turn-on-auto-fill))

(use-package mistty)

(use-package org
  :config
  ;; Configure Org-mode as a word processor
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (let* ((base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-theme-set-faces
     'user
     ;; org-mode headlines and lists
     `(org-document-title ((t (,@headline :height 2.5 :underline nil))))
     `(org-level-1 ((t (,@headline :height 2.0))))
     `(org-level-2 ((t (,@headline :height 1.75))))
     `(org-level-3 ((t (,@headline :height 1.5))))
     `(org-level-4 ((t (,@headline :height 1.25))))
     `(org-level-5 ((t (,@headline))))
     `(org-level-6 ((t (,@headline))))
     `(org-level-7 ((t (,@headline))))
     `(org-level-8 ((t (,@headline))))
     ;; org-mode structural elements
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-document-info ((t (:inherit fixed-pitch))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-drawer ((t (:inherit fixed-pitch))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-link ((t (:underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))
  :custom
  (org-babel-load-languages '((ditaa . t)
                              (emacs-lisp . t)
                              (shell .t)
                              (sqlite . t)))
  :hook
  ((org-mode . variable-pitch-mode)
   (org-mode . visual-line-mode)
   (org-mode . turn-off-auto-fill)))

;; Cause org markup elements to disappear until cursed over
(use-package org-appear
  :straight (org-appear
             :type git
             :host github
             :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autokeywords t)
  (org-appear-autolinks t)
  (org-appear-delay 0.5)
  ;; broken: https://github.com/awth13/org-appear/issues/58
  ;; (org-hidden-keywords t)
  (org-hide-emphasis-markers t))

;; automatically retrieve files as attachments.
;; TODO: internalize https://orgmode.org/manual/Attachments.html
(use-package org-download)

(use-package org-modern
  :hook (org-mode . org-modern-mode))

;; org-roam configuration
;; Support a notes system structured as
;;   org-roam/
;;   - articles/ -- documents authored for an audience other than myself
;;   - daily/    -- date-oriented entries: journals, work logs, &c.
;;   - inbox.org -- unsorted drop-box for new comments/ideas
;;   - notes/    -- notes taken for myself
;;   - sources/  -- notes taken from source materials -- books, articles, talks, &c.
(use-package org-roam
  ;; force loading org-roam so that the global bindings are registered.
  ;; not sure why autoloads are not sufficient
  :demand t
  :custom
  (org-roam-directory (file-truename "~/repos/braindump/org-roam/"))
  (org-default-notes-file (concat org-roam-directory "inbox.org"))
  (org-capture-templates
   '(("i" "Inbox capture" entry (file org-default-notes-file) "* %?\n")))
  (org-roam-capture-templates
   '(("a" "Article" plain "%?"
      :if-new (file+head "article/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
      :immediate-finish t
      :unnarrowed t)
     ("n" "Note" plain "%?"
      :if-new (file+head "note/${slug}.org" "#+title: ${title}\n")
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

;; Extended pretty printer for elisp. Provides `ppp-sexp` and `ppp-macroexpand`.
(use-package ppp
  :straight (ppp
             :type git
             :host github
             :repo "conao3/ppp.el"))

(use-package protobuf-mode
  :straight (protobuf-mode
              :type git
              :flavor melpa
              :files ("editors/protobuf-mode.el" "protobuf-mode-pkg.el")
              :host github
              :repo "protocolbuffers/protobuf"))

(use-package puppet-mode)

;;; Configure rustic with rust-analyzer
(use-package rustic
  :defines rustic-mode-map
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

(use-package scratch-comment
  :straight (scratch-comment
             :type git
             :host github
             :repo "conao3/scratch-comment.el")
  :bind (:map emacs-lisp-mode-map
              ("C-j" . scratch-comment-eval-sexp)))

(use-package selectrum
  :custom (selectrum-mode t))

(use-package selectrum-prescient
  :custom
  (selectrum-prescient-mode t)
  (prescient-persist-mode t))

(use-package systemd)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.njk\\'" . web-mode)))

;; This is moving into core emacs with v30.
(use-package which-key)

(use-package writeroom-mode
  :custom
  (writeroom-fringes-outside-margins nil)
  (writeroom-mode-line t)
  (writeroom-width 110)
  (writeroom-global-effects nil)
  :hook (org-mode . writeroom-mode))

(use-package yaml-mode)

(use-package yasnippet
  :commands
  (yas-reload-all)
  :blackout yas-minor-mode
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
