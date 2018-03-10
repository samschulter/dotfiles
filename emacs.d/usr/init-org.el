;;
;; org-mode environment
;;

(require 'org)
(require 'htmlize)

;; Resources:
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html

;; - Archiving?


;;;
;;; Globals (keywords, directories, etc.)
;;;

;; defines my custom state sequence
(setq org-todo-keywords '((sequence "TODO" "NEXT" "TODAY" "|" "DONE" "CANCELED")))

;; We might use this config somewhere where my tasks are not stored
(if (file-exists-p "~/Boxcryptor/Dropbox/Data_encrypted/org/")
    (progn
       ;; path to all my task files
       (setq org-directory "~/Boxcryptor/Dropbox/Data_encrypted/org/")
       (load-library "find-lisp")
       (setq org-agenda-files (find-lisp-find-files "~/Boxcryptor/Dropbox/Data_encrypted/org/" "\.org$")))
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

(setq org-tags-match-list-sublevels 'indented)
;(setq org-agenda-todo-list-sublevels nil)
;(setq org-use-tag-inheritance nil)

(setq org-agenda-custom-commands
      '(("d" "My custom agenda view!"
         (( agenda "" )
          ( todo "TODAY" )
          ( todo "NEXT" )
          ( alltodo "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'todo '("NEXT" "TODAY"))) (org-agenda-overriding-header "NON-SCHEDULE/DEADLINE ITEMS:")) )))
        ("w" "List of WAITING items"
         todo "WAITING")
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

        ("t" "TODAY format." entry
         (file "todos.org")
         "* TODAY %?")

        ("d" "Deadline TODO format." entry
         (file "todos.org")
         "* TODO %?
DEADLINE: %t")

        ("p" "Project format." entry
         (file "projects.org")
         "* %? :project:")))
(define-key global-map (kbd "C-c c") 'org-capture)




;;;
;;; Mobile org
;;;
;;; https://mobileorg.github.io/documentation/

;; Set to the name of the file where new notes will be stored
;(setq org-mobile-inbox-for-pull "~/Dropbox/org/todos.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Auto-sync
;; https://stackoverflow.com/questions/8432108/how-to-automatically-do-org-mobile-push-org-mobile-pull-in-emacs
;; Run org-mobile-{pull,push} every time Emacs gets idle for my-org-mobile-sync-secs
;(defvar my-org-mobile-sync-secs (* 60 3))

;(defvar my-org-mobile-sync-timer nil)

;(defun my-org-mobile-sync-pull-and-push ()
;  (org-mobile-pull)
;  (org-mobile-push)
;  (message "org-mobile sync done")
;  )

;(defun my-org-mobile-sync-start ()
;  "Start automated org-mobile syncing"
;  (interactive)
;  (setq my-org-mobile-sync-timer
;        (run-with-idle-timer my-org-mobile-sync-secs t
;                             'my-org-mobile-sync-pull-and-push)))

;(defun my-org-mobile-sync-stop ()
;  "Stop automated org-mobile syncing"
;  (interactive)
;  (cancel-timer my-org-mobile-sync-timer))

;(if (file-exists-p "~/Dropbox/org/")
;    (my-org-mobile-sync-start)
;  nil)




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
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))






;;;
;;; UI customizations
;;;

;; one line should be sufficient as a folding (cycle) separator
;; https://www.reddit.com/r/orgmode/comments/3c4xdk/spacing_between_items_when_trees_are_folded/
(setq org-cycle-separator-lines 1)

;; nice bullet point icons
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; other ending icon for folded items
(setq org-ellipsis " [->]")

;; proper text display
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enables `org-indent-mode' by default
  (add-hook 'org-mode-hook 'visual-line-mode) ; proper text warping
  ;(add-hook 'org-mode-hook 'visual-fill-column-mode) ; warps text at fill-column
  )

;; Don't let Monokai change the font sizes
;; https://emacs.stackexchange.com/questions/22584/disable-enlarged-org-mode-header-appearance
(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'my/org-mode-hook)
