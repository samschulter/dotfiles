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

;; defines my custom state sequence
(setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE")))

;; We might use this config somewhere where my tasks are not stored
(if (file-exists-p "~/Boxcryptor/Dropbox/data_encrypted/org/")
    (progn
       ;; path to all my task files
       (setq org-directory "~/Boxcryptor/Dropbox/data_encrypted/org/")
      (add-to-list 'org-agenda-files (expand-file-name "~/Boxcryptor/Dropbox/data_encrypted/org"))
      )
  nil)



;;;
;;; Agenda
;;;

;; global key to open the agenda
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-agenda-window-setup 'current-window)

;; span of the agenda (show agenda from yesterday for 5 days)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 5)

;; remove scheduled DONE items in agenda
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(setq org-tags-match-list-sublevels 'indented)

(setq org-agenda-custom-commands
      '(("d" "My custom agenda view!"
         (( agenda "" )
          ( todo "NEXT" )
          ( todo "WAIT" )
          ( alltodo "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'todo '("NEXT" "WAIT"))) (org-agenda-overriding-header "NON-SCHEDULE/DEADLINE ITEMS:")) )))
        ("w" "List of WAIT items"
         todo "WAIT")
        ("r" "Closed items in the last 35 days"
         agenda ""
         ((org-agenda-start-day "-35d")
          (org-agenda-span 35)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-with-log-mode t)
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'nottodo 'done))
          ))
        ))

;;;
;;; Capture
;;;

;; defines a single template and wraps the selection of it in the capture buffer
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-capture-templates
      '(("a" "TODO format." entry
         (file "todos.org")
         "* TODO %?")

        ("s" "Scheduled TODO format." entry
         (file "todos.org")
         "* TODO %?
SCHEDULED: %t")

        ("n" "NEXT format." entry
         (file "todos.org")
         "* NEXT %?")

        ("d" "Deadline TODO format." entry
         (file "todos.org")
         "* TODO %?
DEADLINE: %t")

        ("p" "Project format." entry
         (file "projects.org")
         "* %? :project:")))
(define-key global-map (kbd "C-c c") 'org-capture)




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
  (local-set-key "\M-n" 'org-metaup)))

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



;;
;; todoist.com integration
;;
(require 'todoist)
(setq todoist-token "40dbe75129b24c147abf5780c7156926addc6c8e")
(setq url-mime-encoding-string "identity")
