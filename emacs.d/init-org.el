(require 'org)
(require 'htmlize)

;; Resources:
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html


;; - What about archiving?


;;;
;;; Globals (keywords, directories, etc.)
;;;

;; defines my custom state sequence
;; (everything behind the pipe symbol "|" are considered as complete tasks)
(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED")))

;; path to all my task files
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files '("~/Dropbox/org/"))



;;;
;;; Agenda
;;;

;; global key to open the agenda
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-agenda-window-setup 'current-window)

;; span of the agenda (show agenda from yesterday for 5 days)
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)
(setq org-agenda-start-on-weekday nil)

;; remove scheduled DONE items in agenda
;;(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-custom-commands
      '(("d" "My custom agenda view!"
         ((agenda "")
          (alltodo "")))))
;;(setq org-agenda-custom-commands
;;      '(("d" "My custom agenda view!"
;;         ((agenda "")
;;          (alltodo ""
;;                   ((org-agenda-skip-function '(org-agenda-skip-if 'scheduled 'deadline))
;;                    (org-agenda-overriding-header "Non-schedule/deadline items:")))))))

;; TODO: tweak this agenda view: can we only show non-scheduled items in the lower part?
;; TODO: we eventually should create one ideas.org file that is really just for prototype ideas; as soon as we have something more concrete, we should create a separate file for it? Each todo is clearly assigned to projects.





;;;
;;; Mobile org
;;;
;;; https://mobileorg.github.io/documentation/

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/todos.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Auto-sync
;; https://stackoverflow.com/questions/8432108/how-to-automatically-do-org-mobile-push-org-mobile-pull-in-emacs
;; Run org-mobile-{pull,push} every time Emacs gets idle for my-org-mobile-sync-secs
(defvar my-org-mobile-sync-secs (* 60 3))

(defvar my-org-mobile-sync-timer nil)

(defun my-org-mobile-sync-pull-and-push ()
  (org-mobile-pull)
  (org-mobile-push)
  (message "org-mobile sync done")
  )

(defun my-org-mobile-sync-start ()
  "Start automated org-mobile syncing"
  (interactive)
  (setq my-org-mobile-sync-timer
        (run-with-idle-timer my-org-mobile-sync-secs t
                             'my-org-mobile-sync-pull-and-push)))

(defun my-org-mobile-sync-stop ()
  "Stop automated org-mobile syncing"
  (interactive)
  (cancel-timer my-org-mobile-sync-timer))

(my-org-mobile-sync-start)



;;;
;;; Capture
;;;

;; defines a single template and wraps the selection of it in the capture buffer
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "todos.org")
         "* TODO %?
SCHEDULED: %t")))
(defun sjs-org-task-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "a"))
(define-key global-map (kbd "C-c c") 'sjs-org-task-capture)

;; TODO: we should have more templates:
;; TODOs: non-scheduled, scheduled, deadline
;; Just notes: capture something (eventually with a date?) in a separate file, e.g., notes or resources or links??? We may store websites, papers, links, etc. in this file?






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
  (add-hook 'org-mode-hook 'visual-fill-column-mode)) ; warps text at fill-column
