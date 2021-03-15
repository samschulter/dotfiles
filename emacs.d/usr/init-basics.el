;;
;; collection of customizations for builtin emacs functions
;;

;; set default fill column (e.g., used in AucTeX mode)
(setq-default fill-column 80)

;; downgrades important questions (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; remove toolbar
(tool-bar-mode -1)

;; inhibits start screen and shows *scratch*
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;; the *scratch* buffer should be org-mode
;; https://emacs.stackexchange.com/questions/16492/is-it-possible-to-create-an-org-mode-scratch-buffer
(setq initial-major-mode 'org-mode)

;; backup files: http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-backup-files")))
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
;;(setq ring-bell-function 'ignore)

;; default indentation of 4 spaces
(setq-default indent-tabs-mode nil) ; no TAB for indent
(setq-default tab-width 4)
(setq sh-basic-offset 4)
(setq sh-indentation 4)

;; highlights trailing whitespace
(setq-default show-trailing-whitespace t)

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

;; replace shortcut to "kill-buffer" with "kill-this-buffer". I don't want to get
;; asked which buffer to kill, most of the time I want to kill the current one.
;; source: http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)

;; insert closing bracket automatically
;; more info: http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(electric-pair-mode 1)

;; CamelCase for forward-word and backward-word
;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'org-mode-hook 'subword-mode)

