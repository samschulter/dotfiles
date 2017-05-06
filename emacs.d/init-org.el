(require 'org)
(require 'htmlize)

(add-hook 'org-mode-hook (lambda ()
  ;; Dvorak.
  (local-set-key "\M-h" 'org-do-promote)
  (local-set-key "\M-s" 'org-do-demote)
  (local-set-key (kbd "M-S-h") 'org-promote-subtree)
  (local-set-key (kbd "M-S-s") 'org-demote-subtree)

  (local-set-key "\M-t" 'org-metadown)
  (local-set-key "\M-n" 'org-metaup)

  (local-set-key (kbd "C-c a") 'org-agenda)

  (local-set-key (kbd "C-S-<return>") 'org-insert-heading-after-current)))

(setq org-startup-with-inline-images t
      org-src-fontify-natively t
      org-latex-to-pdf-process (list "latexmk -pdf %f")
      org-image-actual-width 600
      org-agenda-skip-scheduled-if-done t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-babel-python-command "python3")

;; Automatic link description downloading.
;; Source: https://github.com/pkkm/.emacs.d/blob/8ac745bae09e303a13708fb9260849de9e1d8502/conf/mode-specific/org.el#L90

; (use-package s :ensure t :commands s-trim s-collapse-whitespace)
(autoload 'mm-url-decode-entities-string "mm-url")
(defun get-url-html-title (url &rest ignored)
"Return the title of the HTML page at URL."
(let ((download-buffer (url-retrieve-synchronously url))
        title-start title-end)
    (save-excursion
    (set-buffer download-buffer)
    (beginning-of-buffer)
    (setq title-start (search-forward "<title>"))
    (search-forward "</title>")
    (setq title-end (search-backward "<"))
    (s-trim
        (s-collapse-whitespace
        (mm-url-decode-entities-string
        (buffer-substring-no-properties title-start title-end)))))))

(defun my-org-toggle-auto-link-description ()
"Toggle automatically downloading link descriptions."
(interactive)
(if org-make-link-description-function
    (progn
        (setq org-make-link-description-function nil)
        (message "Automatic link description downloading disabled."))
    (setq org-make-link-description-function #'get-url-html-title)
    (message "Automatic link description downloading enabled.")))

;; Source: http://stackoverflow.com/a/27043756
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

;; Source: http://emacs.stackexchange.com/questions/22584/disable-enlarged-org-mode-header-appearance
(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'my/org-mode-hook)
;; The second-level color overlaps with the DONE color in the monokai theme, so we manually replace it.
;; Source: http://emacs.stackexchange.com/questions/26781/customize-colors-of-level-in-org-mode
;;(custom-theme-set-faces 'user
;;                        `(org-level-2 ((t (:foreground "magenta")))))


;; one line should be sufficient as a folding (cycle) separator
;; https://www.reddit.com/r/orgmode/comments/3c4xdk/spacing_between_items_when_trees_are_folded/
(setq org-cycle-separator-lines 1)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-ellipsis " [->]")

(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook 'visual-line-mode))
