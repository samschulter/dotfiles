(require 'org)
(require 'htmlize)

;; Most of the this configuration comes from:
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html



;;;
;;; Globals (keywords, directories, etc.)
;;;

;; defines my custom state sequence
;; (everything behind the pipe symbol "|" are considered as complete tasks)
(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED")))

;; path to all my task files
(setq org-directory "~/Google Drive/org/")
(setq org-agenda-files '("~/Google Drive/org/"))



;;;
;;; Agenda
;;;

;; global key to open the agenda
(define-key global-map (kbd "C-c a") 'org-agenda-list)

;; TODO: define proper keys for the agenda view!
;; see C-h m in agenda view for all commands



;;;
;;; Capture
;;;

;; defines a single template and wraps the selection of it in the capture buffer
;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "tasks.org")
         "* TODO %?
SCHEDULED: %t")))
(defun sjs-org-task-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "a"))
(define-key global-map (kbd "C-c c") 'sjs-org-task-capture)



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
