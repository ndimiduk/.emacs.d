;; will not pull in latest and greatest org-mode as an older version
;; shipps with emacs. likely need to install manually.
(ensure-packages '(org))
(eval-after-load 'org
  '(progn
     (setq org-directory "~/Dropbox/Documents/org")
     (org-remember-insinuate)
     (global-set-key (kbd "C-M-r") 'org-capture)
     (setq org-src-fontify-natively t)
     (setq org-src-tab-acts-natively t)))

;; Steve Yegge's js2-mode is full of hotness, but is it maintained any longer..?
;; http://steve-yegge.blogspot.com/2008/03/js2-mode-new-javascript-mode-for-emacs.html
(ensure-packages '(js2-mode))

;; jrb files for scripting the HBase shell
(push '("\\.jrb$" . ruby-mode) auto-mode-alist)

;; pull in thrift-mode. it would be great to move this to marmalade...
(require 'thrift-mode)
(push '("\\.thrift$" . thrift-mode) auto-mode-alist)

;; puppet!
(ensure-packages '(puppet-mode))
(push '("\\.pp$" . puppet-mode) auto-mode-alist)
