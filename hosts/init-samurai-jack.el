;;; package --- Summary
;;; Commentary:
;;; Code:

(when (display-graphic-p)
  (toggle-frame-maximized (selected-frame)))

(setopt org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

;; WSL with emacs-29 inspired by
;; https://emacsredux.com/blog/2021/12/19/using-emacs-on-windows-11-with-wsl2/
;; WSL specifics
;; https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  ;; teach emacs how to open links in the native browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser  'browse-url-generic))))

(provide 'init-samurai-jack)
;;; init-samurai-jack.el ends here
