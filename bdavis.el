(defun stigmatize ()
  "Switch the solarized color theme based on the ambient light sensor."
  (if (< 1000000 (string-to-number (shell-command-to-string "stigma")))
      (color-theme-solarized-light)
    (color-theme-solarized-dark)))

(run-with-timer 1 10 'stigmatize)
