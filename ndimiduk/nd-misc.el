;;;
;; misc.el -- miscellany.
;;

(column-number-mode t)
(put 'upcase-region 'disabled nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
(set-default 'fill-column 79)
(global-hl-line-mode t)

;; no more tabs
(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 4)
(set-default 'c-basic-offset 2)

(push '("Rakefile" . ruby-mode) auto-mode-alist)
(push '("\\.jrb$" . ruby-mode) auto-mode-alist)
(push '("\\.md$" . markdown-mode) auto-mode-alist)
(push '("\\.markdown$" . markdown-mode) auto-mode-alist)
(push '("\\.yml$" . yaml-mode) auto-mode-alist)
(push '("\\.yaml$" . yaml-mode) auto-mode-alist)
(push '("\\.pp$" . puppet-mode) auto-mode-alist)
(push '("\\.js$" . js2-mode) auto-mode-alist)
(push '("\\.json$" . js2-mode) auto-mode-alist)

;; use smex most always
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(provide 'nd-misc)
