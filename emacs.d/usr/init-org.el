;;
;; org-mode environment
;;
(require 'org)
(require 'htmlize)

;; Resources:
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html


;;;
;;; Archiving
;;;
(setq org-archive-location "%s_archive::datetree/")


;;;
;;; Globals (keywords, directories, etc.)
;;;

;; custom todo state sequence
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))

;; path to all my task files
;;(setq org-directory "~/.emacs.d/todos")
;;(add-to-list 'org-agenda-files (expand-file-name "~/.emacs.d/todos"))


;;;
;;; Agenda
;;;

;; global key to open the agenda
;;(define-key global-map (kbd "C-c a") 'org-agenda)
;;(setq org-agenda-window-setup 'current-window)

;; span of the agenda (show agenda from yesterday for 5 days)
;;(setq org-agenda-start-on-weekday nil)
;;(setq org-agenda-span 5)

;; remove scheduled DONE items in agenda
;;(setq org-agenda-skip-scheduled-if-done t)
;;(setq org-agenda-skip-deadline-if-done t)

;;(setq org-tags-match-list-sublevels 'indented)

;;(setq org-agenda-custom-commands
;;      '(("d" "Custom agenda view"
;;         (( todo "NEXT" )
;;          ( todo "TODO" )
;;          ( todo "WAIT" )))
;;        ("b" "Backlog"
;;         todo "BACKLOG")
;;        ("r" "Closed in last 35 days"
;;         agenda ""
;;         ((org-agenda-start-day "-35d")
;;          (org-agenda-span 35)
;;          (org-agenda-start-on-weekday nil)
;;          (org-agenda-start-with-log-mode t)
;;          (org-agenda-skip-function
;;           '(org-agenda-skip-entry-if 'nottodo 'done))
;;          ))
;;        ))


;;;
;;; Capture
;;;

;; defines a single template and wraps the selection of it in the capture buffer
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
;;(setq org-capture-templates
;;      '(("a" "TODO format." entry
;;         (file "todos.org")
;;         "* TODO %?")
;;
;;        ("b" "BACKLOG format." entry
;;         (file "todos.org")
;;         "* BACKLOG %?")))

;;(define-key global-map (kbd "C-c c") 'org-capture)


;;;
;;; Tweaks
;;;

;; subtree todos need to be done first!
(setq org-enforce-todo-dependencies t)

;; add timestamp when task changes to done
(setq org-log-done (quote time))

;; add timestamp when I postpone tasks (deadline or scheduled) ...
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

;; define some keybindings for org-mode to move items
(add-hook 'org-mode-hook (lambda ()
  (local-set-key "\M-h" 'org-do-promote)
  (local-set-key "\M-s" 'org-do-demote)
  (local-set-key (kbd "M-S-h") 'org-promote-subtree)
  (local-set-key (kbd "M-S-s") 'org-demote-subtree)
  (local-set-key "\M-t" 'org-metadown)
  (local-set-key "\M-n" 'org-metaup)
  (local-set-key (kbd "RET") 'org-return-and-maybe-indent)
  (local-set-key (kbd "<C-return>") 'org-meta-return)))

;; when embedding source code, apply proper font styles
(setq org-src-fontify-natively t)

;; command for pdf export
(setq org-latex-to-pdf-process (list "latexmk -pdf %f"))

;; in case we ever embed images into an org file:
;;(setq org-startup-with-inline-images t)
;;(setq org-image-actual-width 600)

;; for org-refile:
;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq org-refile-targets '((org-agenda-files :maxlevel . 6)))

;; spell checker for org-mode
(add-hook 'org-mode-hook 'flyspell-mode)


;;;
;;; UI customizations
;;;

;; one line should be sufficient as a folding (cycle) separator
;; https://www.reddit.com/r/orgmode/comments/3c4xdk/spacing_between_items_when_trees_are_folded/
(setq org-cycle-separator-lines 1)

;; other ending icon for folded items
(setq org-ellipsis " [...]")

;; proper text display
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enables `org-indent-mode' by default
  (add-hook 'org-mode-hook 'visual-line-mode) ; proper text warping
  )

;; Github-style markdown exporter for org
;; https://github.com/larstvei/ox-gfm
(eval-after-load "org"
  '(require 'ox-gfm nil t))


;; Journal
(customize-set-variable 'org-journal-dir "~/.emacs.d/journal")
(customize-set-variable 'org-journal-date-format "%A, %d %B %Y")
(customize-set-variable 'org-journal-file-type 'yearly)
(require 'org-journal)
(global-set-key (kbd "C-x C-j") 'org-journal-new-entry)
;; This does not work ...
;;(add-hook 'org-journal-hook (lambda ()
;;  (local-set-key "C-c C-b" 'org-backward-heading-same-level)
;;  (local-set-key "C-c C-f" 'org-forward-heading-same-level)))

;; This almost works ... but find-file does open a dired buffer instead of the file ...
;;(defun get-current-journal-file ()
;;  "Gets current journal filename."
;;  (let* (this-monday (get-current-journal-file))
;;    (expand-file-name (concat org-journal-dir this-monday))))
;;
;;(defun journal-file-current ()
;;  "Creates and loads the current journal file. This is only to open it.
;;To capture a new note, use 'org-journal-new-entry'"
;;  (interactive)
;;  (find-file (get-current-journal-file)))
