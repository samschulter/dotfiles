;; adds line numbers on the left side of the buffer
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
;; downgrades important questions (yes/no) to (y/n)
;;(fset 'yes-or-no-p 'y-or-n-p) ; yes/no -> y/n
;; version-control follow symlinks
;;(setq vc-follow-symlinks t) ; Always follow symlinks.
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

;; no need for scroll bars
(scroll-bar-mode -1)

;; When point leaves window, only scroll until point instead of re-centering
(setq scroll-conservatively 100)

;; change keybord shortcuts of C-a and M-m
;;back-to-indentation
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

;; Don't ring the bell when I type something wrong
;;(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; default indentation
(setq-default indent-tabs-mode nil) ; no TAB for indent
(setq-default tab-width 2)(setq sh-basic-offset 2)(setq sh-indentation 2)

;; as the names say, highlights trailing whitespace and deletes it when file is saved
(setq-default show-trailing-whitespace t)

;; Removes menu and toolbar when in GUI mode ...
;;(menu-bar-mode -1)
(tool-bar-mode -1)

;; https://github.com/abo-abo/ace-window
;; (part of Emacs)
(global-set-key (kbd "M-p") 'ace-window)


;; refresh the whole screen
(global-set-key (kbd "C-c d") 'redraw-display)

;;(setq custom-file "~/.emacs.d/custom.el")
;;(load custom-file 'noerror)


;; remove vc-git mode (it's soooo slow on a mounted network device)
;;(require 'vc)
;;(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; http://snak.tumblr.com/post/4203099162/disable-vc-mode
(setq vc-handled-backends nil)


;; Override selected text when starting to type, instead of appending the new text
(delete-selection-mode t)

;; End every file with a newline
(setq require-final-newline t)

;; It still happens too often ... ask me before closing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Auto-refresh buffer if file changes outside of emacs
(global-auto-revert-mode t)

;; replace shortcut to "kill-buffer" with "kill-this-buffer"
;; source: http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)
