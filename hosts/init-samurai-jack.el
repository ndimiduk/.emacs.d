;;; package --- Summary
;;; Commentary:
;;; Code:

(when (display-graphic-p)
  (toggle-frame-maximized (selected-frame)))

(setopt org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

(use-package tramp
  :demand t
  :custom
  (small-temporary-file-directory       (expand-file-name "~/.emacs.d/tmp/"))
  (tramp-auto-save-directory            (expand-file-name "~/.emacs.d/tmp/tramp-auto-save/"))
  (tramp-persistency-file-name          (expand-file-name "~/.emacs.d/tmp/tramp-connection-history"))
  :config
  (make-directory small-temporary-file-directory t)
  (set-file-modes small-temporary-file-directory #o700)
  (make-directory tramp-auto-save-directory t)
  (set-file-modes tramp-auto-save-directory #o700))

(provide 'init-samurai-jack)
;;; init-samurai-jack.el ends here
