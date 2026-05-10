;; From https://github.com/raxod502/straight.el#getting-started
;;
;; Users of Emacs versions >= 27 will want to add:
(setq package-enable-at-startup nil)
;; to their early init-file to prevent package.el loading packages prior to their init-file loading.

;; Use find(1) instead of git to detect modified packages — much faster than
;; the default git-based check.
(setq straight-check-for-modifications '(find-at-startup find-when-checking))
(setq use-package-always-defer t)

;; On systems where /home is a symlink (e.g. Bazzite, where /home -> /var/home),
;; normalize HOME to its true path before anything else constructs ~ -based paths.
;; No-op on systems where /home is not a symlink.
(setenv "HOME" (file-truename (getenv "HOME")))
(setq abbreviated-home-dir nil)
