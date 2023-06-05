(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     default))
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
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; initialize host-specific configs, a la
;; https://nicolas.petton.fr/blog/per-computer-emacs-settings.html
(defvar hostname (substring (shell-command-to-string "hostname") 0 -1))
(defvar host-dir "~/.emacs.d/hosts/")
(add-to-list 'load-path host-dir)
(let ((init-host-feature (intern (downcase (concat "init-" hostname)))))
  (require init-host-feature nil 'noerror))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package flyspell
  :init
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list"))

(use-package hl-line
  :init (global-hl-line-mode t))

(use-package magit
  :init
  (setq vc-handled-backends nil)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  :bind (("C-c g" . 'magit-status)
         ("C-x g" . 'magit-status)))

(use-package color-theme-sanityinc-solarized
  :init (when window-system
	  (color-theme-sanityinc-solarized-light)))

(use-package selectrum
  :init (selectrum-mode +1))
(use-package selectrum-prescient
  :init (progn
	  (selectrum-prescient-mode +1)
	  (prescient-persist-mode +1)))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-<" . company-select-first)
        ("M->" . company-select-last)
        ("M-/" . company-complete)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package puppet-mode)
(use-package yaml-mode)

(use-package markdown-mode
  :init
  (add-hook 'markdown-mode-hook #'turn-on-auto-fill))

(use-package dockerfile-mode)

;(use-package bash-completion
;  :config (add-hook 'shell-dynamic-complete-functions
;                    'bash-completion-dynamic-complete))

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
  :config
  (setq rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (setq rustic-format-on-save t))

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-snippet nil
        lsp-yaml-schema-store-local-db "~/.emacs.d/var/lsp/lsp-yaml-schemas.json"
        lsp-yaml-schemas '((kubernetes . ["base/*.yaml"
                                          "overlays/**/*.yaml"])
                           (http://json\.schemastore\.org/kustomization . ["*ustomization.yaml"])))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'yaml-mode-hook 'lsp-mode)
  (add-hook 'docker-hook 'lsp-mode)
  (add-hook 'js-json-mode-hook 'lsp-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
(use-package company-lsp :commands company-lsp)

;; clojure-mode and friends
(use-package clojure-mode)
(use-package cider)
