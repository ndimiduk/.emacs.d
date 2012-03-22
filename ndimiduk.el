(ensure-packages '(org))
(eval-after-load 'org
  '(progn
     (setq org-directory "~/Dropbox/Documents/org")
     (org-remember-insinuate)
     (global-set-key (kbd "C-M-r") 'org-capture)))
