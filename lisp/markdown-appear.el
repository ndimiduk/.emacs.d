;;; markdown-appear.el --- Auto-reveal hidden markdown markup at point -*- lexical-binding: t; -*-

;; Modeled after org-appear. Hides markdown markup (emphasis delimiters,
;; link brackets, heading markers, etc.) but reveals it when the cursor
;; enters the element. Uses markdown-mode's existing font-lock faces to
;; detect element boundaries — no parser required.

(require 'markdown-mode)
(require 'cl-lib)

(defgroup markdown-appear nil
  "Auto-reveal hidden markdown markup at point."
  :group 'markdown)

(defcustom markdown-appear-delay 0.0
  "Seconds of idle time before revealing markup.  0 means instant."
  :type 'number
  :group 'markdown-appear)

(defcustom markdown-appear-max-region 500
  "Maximum region size (chars) to reveal.  Prevents expanding into
large code blocks."
  :type 'integer
  :group 'markdown-appear)

;;; --- face sets used for element detection ---

(defvar markdown-appear--content-faces
  '(markdown-bold-face
    markdown-italic-face
    markdown-bold-italic-face
    markdown-inline-code-face
    markdown-code-face
    markdown-link-face
    markdown-url-face
    markdown-plain-url-face
    markdown-reference-face
    markdown-header-face
    markdown-header-face-1 markdown-header-face-2
    markdown-header-face-3 markdown-header-face-4
    markdown-header-face-5 markdown-header-face-6
    markdown-strike-through-face
    markdown-math-face
    markdown-blockquote-face
    markdown-hr-face
    markdown-list-face
    markdown-footnote-marker-face
    markdown-footnote-text-face)
  "Faces on element content (triggers reveal when cursor is here).")

(defvar markdown-appear--all-faces
  (append '(markdown-markup-face
            markdown-header-delimiter-face
            markdown-line-break-face
            markdown-language-keyword-face
            markdown-language-info-face)
          markdown-appear--content-faces)
  "All faces belonging to a markdown element (content + delimiters).")

;;; --- internal state ---

(defvar-local markdown-appear--prev-region nil
  "Cons (BEG . END) of the currently-revealed element, or nil.")

(defvar-local markdown-appear--timer nil
  "Pending idle timer for delayed reveal.")

(defvar-local markdown-appear--saved-hide-markup nil
  "Value of `markdown-hide-markup' before the mode was enabled.")

;;; --- helpers ---

(defun markdown-appear--faces-at (&optional pos)
  "Return faces at POS as a list."
  (let ((f (get-text-property (or pos (point)) 'face)))
    (cond ((null f) nil)
          ((listp f) f)
          (t (list f)))))

(defun markdown-appear--in-element-p (&optional pos)
  "Non-nil if POS is inside a markdown element."
  (cl-intersection (markdown-appear--faces-at pos)
                   markdown-appear--all-faces))

(defun markdown-appear--element-region ()
  "Return (BEG . END) of the markdown element at point, or nil.
Expands outward while adjacent characters carry element faces."
  (when (markdown-appear--in-element-p)
    (let ((beg (point))
          (end (point)))
      (while (and (> beg (point-min))
                  (markdown-appear--in-element-p (1- beg)))
        (cl-decf beg))
      (while (and (< end (point-max))
                  (markdown-appear--in-element-p end))
        (cl-incf end))
      (when (<= (- end beg) markdown-appear-max-region)
        (cons beg end)))))

(defun markdown-appear--show (region)
  "Reveal hidden markup in REGION."
  (with-silent-modifications
    (remove-text-properties (car region) (cdr region)
                            '(invisible nil display nil))
    (decompose-region (car region) (cdr region))))

(defun markdown-appear--hide (region)
  "Re-hide markup in REGION via re-fontification."
  (font-lock-flush (car region) (cdr region))
  (font-lock-ensure (car region) (cdr region)))

(defun markdown-appear--reveal (region)
  "Reveal REGION and record it as current."
  (markdown-appear--show region)
  (setq markdown-appear--prev-region region))

;;; --- core loop ---

(defun markdown-appear--post-cmd ()
  "Post-command handler: reveal markup at point, hide previous."
  (when markdown-appear--timer
    (cancel-timer markdown-appear--timer)
    (setq markdown-appear--timer nil))
  (let ((region (markdown-appear--element-region)))
    (cond
     ;; still in same element — nothing to do
     ((equal region markdown-appear--prev-region))
     ;; entered a new element
     (region
      (when markdown-appear--prev-region
        (markdown-appear--hide markdown-appear--prev-region))
      (if (> markdown-appear-delay 0)
          (let ((buf (current-buffer)))
            (setq markdown-appear--timer
                  (run-with-idle-timer
                   markdown-appear-delay nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (markdown-appear--reveal region)))))))
        (markdown-appear--reveal region)))
     ;; left an element
     (markdown-appear--prev-region
      (markdown-appear--hide markdown-appear--prev-region)
      (setq markdown-appear--prev-region nil)))))

;;; --- minor mode ---

;;;###autoload
(define-minor-mode markdown-appear-mode
  "Auto-reveal hidden markdown markup at point.
Enables `markdown-hide-markup' and adds a post-command hook that
reveals markup delimiters when the cursor enters an element."
  :lighter " md-appear"
  :group 'markdown-appear
  (if markdown-appear-mode
      (progn
        (setq markdown-appear--saved-hide-markup markdown-hide-markup
              markdown-hide-markup t)
        (add-to-invisibility-spec 'markdown-markup)
        (markdown-reload-extensions)
        (add-hook 'post-command-hook #'markdown-appear--post-cmd nil t))
    (remove-hook 'post-command-hook #'markdown-appear--post-cmd t)
    (when markdown-appear--timer
      (cancel-timer markdown-appear--timer)
      (setq markdown-appear--timer nil))
    (when markdown-appear--prev-region
      (markdown-appear--hide markdown-appear--prev-region)
      (setq markdown-appear--prev-region nil))
    (setq markdown-hide-markup markdown-appear--saved-hide-markup)
    (if markdown-hide-markup
        (add-to-invisibility-spec 'markdown-markup)
      (remove-from-invisibility-spec 'markdown-markup))
    (markdown-reload-extensions)))

(provide 'markdown-appear)
;;; markdown-appear.el ends here
