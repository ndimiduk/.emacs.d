;;;
;; init file, loaded before installed packages.
;;

;; enable to debug system and user init files.
;; (setq debug-on-error 't)

;; fix Quit: "empty or unsupported pasteboard type" with 24.3
;; https://groups.google.com/forum/#!topic/gnu.emacs.help/2WIkga774vM
(when (eq window-system 'ns)
  (defadvice ns-get-pasteboard (around hack-empty-pasteboard compile activate)
    (condition-case err
        ad-do-it
      (quit (message "%s" (cadr err))
            nil))))

;; following snippets inspired by technomancy/emacs-starter-kit 2.x

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; uncomment for < 23.0
;;(setq user-emacs-directory "~/.emacs.d/")

;; establish system and user init files, run via 'after-init-hook so that they
;; have access to the initialized package manager.
(setq nd-system-config (concat user-emacs-directory (car (split-string system-name "\\.")) ".el")
      nd-user-config (concat user-emacs-directory user-login-name ".el")
      nd-user-dir (concat user-emacs-directory user-login-name))

(add-to-list 'load-path nd-user-dir)

(defun load-when-exists (file-or-dir)
  "load a file or the contents of a directory."
  (if (file-exists-p file-or-dir)
      (progn
        (message "%s exists, loading..." file-or-dir)
        (if (file-directory-p file-or-dir)
            (mapc 'load (directory-files file-or-dir nil "^[^#].*el$"))
          (load file-or-dir)))
    (message "%s does not exist." file-or-dir)))

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a byte-compiled lambda doesn't already exist in the list.

(defun nd-load-system-config () (load-when-exists nd-system-config))
(defun nd-load-user-config () (load-when-exists nd-user-config))
(defun nd-load-user-dir () (load-when-exists nd-user-dir))

;; Hooks list appears to operate as a stack
(add-hook 'after-init-hook 'nd-load-user-config)
(add-hook 'after-init-hook 'nd-load-user-dir)
(add-hook 'after-init-hook 'nd-load-system-config)
