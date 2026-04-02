;;; package --- Summary
;;; Commentary:
;;; Code:

(when (display-graphic-p)
  (set-frame-size (selected-frame) 185 52))

(setopt org-ditaa-jar-path "/opt/homebrew/opt/ditaa/libexec/ditaa-0.11.0-standalone.jar")

(setopt repos-dir (file-truename "~/src/"))

(put 'org-task-id-prefix 'safe-local-variable #'stringp)
(put 'org-task-id-counter 'safe-local-variable #'integerp)
(put 'lsp-java-vmargs 'safe-local-variable #'listp)


(provide 'init-hjgfjv2ll9)
;;; init-hjgfjv2ll9.el ends here
