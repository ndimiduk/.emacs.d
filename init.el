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
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(straight-use-package-by-default t)
 '(straight-vc-git-default-clone-depth 1)
 '(tool-bar-mode nil))
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
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(when (display-graphic-p)
  (set-frame-size (selected-frame) 202 60))

(use-package hl-line
  :init (global-hl-line-mode t))

(use-package magit
  :init (setq vc-handled-backends nil)
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

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind (("M-/" . 'company-complete)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))
(use-package flycheck
  :init (global-flycheck-mode))

(use-package puppet-mode)
