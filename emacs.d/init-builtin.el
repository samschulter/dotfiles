;;
;; collection of customizations for builtin emacs functions
;;


;; copy content of whole buffer
(defun copy-all ()
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Copied to clipboard."))
(global-set-key (kbd "C-c C-a") 'copy-all)

;; adds line numbers on the left side of the buffer
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; downgrades important questions (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; remove toolbar
(tool-bar-mode -1)

;; inhibits start screen and shows *scratch*
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; backup files: http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.emacs_backup_files")))
(setq backup-by-copying t)

;; highlights beginning/ending parenthesis
;; more options here: https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; scrolling stops only if point is at last position
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Scrolling.html
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)
;; When point leaves window, only scroll until point instead of re-centering
(setq scroll-conservatively 100)
;; no need for scroll bars
(scroll-bar-mode -1)

;; change keybord shortcuts of C-a and M-m
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

;; don't ring the bell when I type something wrong
(setq ring-bell-function 'ignore)

;; default indentation
(setq-default indent-tabs-mode nil) ; no TAB for indent
(setq-default tab-width 2)(setq sh-basic-offset 2)(setq sh-indentation 2)

;; highlights trailing whitespace
(setq-default show-trailing-whitespace t)

;; refresh the whole screen
(global-set-key (kbd "C-c d") 'redraw-display)

;; disable vc mode (it's soooo slow on a mounted network device)
;; http://snak.tumblr.com/post/4203099162/disable-vc-mode
(setq vc-handled-backends nil)

;; override selected text when starting to type, instead of appending the new text
(delete-selection-mode t)

;; end every file with a newline
(setq require-final-newline t)

;; it still happens too often ... ask me before closing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; auto-refresh buffer if file changes outside of emacs
(global-auto-revert-mode t)

;; replace shortcut to "kill-buffer" with "kill-this-buffer"
;; source: http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)
