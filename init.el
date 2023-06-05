(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(sanityinc-solarized-light))
 '(custom-safe-themes
   '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
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

(when (display-graphic-p)
  (set-frame-size (selected-frame) 202 60))

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
  :init (if (null window-system)
	    (color-theme-sanityinc-solarized-light)
	  (color-theme-sanityinc-solarized-dark)))

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

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-command)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-<" . company-select-first)
        ("M->" . company-select-last)
        ("M-/" . company-complete))
  (:map company-mode-map
        ("[?\t]" . tab-indent-or-complete)))

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
  (setq lsp-enable-snippet nil)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
