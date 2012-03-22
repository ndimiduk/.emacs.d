(defun stigmatize ()
  "Switch the solarized color theme based on the ambient light sensor."
  (interactive)
  (if (< 1000000 (string-to-number (shell-command-to-string "stigma")))
      (color-theme-solarized-light)
    (color-theme-solarized-dark)))

(run-with-timer 1 10 'stigmatize)

(ensure-packages '(rainbow-delimiters))
(eval-after-load 'rainbow-delimiters
  '(global-rainbow-delimiters-mode))
