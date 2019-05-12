;;
;; collection of customizations for builtin emacs functions
;;

;; set default fill column (e.g., used in AucTeX mode)
(setq-default fill-column 80)

;; adds line numbers on the left side of the buffer
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; downgrades important questions (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; remove toolbar
(tool-bar-mode -1)

;; inhibits start screen and shows *scratch*
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

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
(setq ring-bell-function 'ignore)

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

;; spell checker - requires ispell to be installed
(setq exec-path (append exec-path '("/usr/local/bin")))
;; emacs needs a path to executable 'ispell'
;; https://sdqali.in/blog/2012/05/04/fixing-flyspell-for-emacs-in-mac-os-x/
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;; remembers last edit point when re-opening a file:
;; https://www.emacswiki.org/emacs/SavePlace
;(require 'saveplace)
;(setq save-place-file (concat user-emacs-directory "saveplace-history") )
;(setq-default save-place t)

;; ace-window
;; easily switch between multiple windows
;(global-set-key (kbd "M-p") 'ace-window)

;; CamelCase for forward-word and backward-word
;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'org-mode-hook 'subword-mode)

;; the *scratch* buffer should be org-mode
;; https://emacs.stackexchange.com/questions/16492/is-it-possible-to-create-an-org-mode-scratch-buffer
(setq initial-major-mode 'org-mode)






;;
;; Helm navigation
;;
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-x") 'helm-all-mark-rings)

;; lets replace isearch-backward with helm-resume (more often used)
(global-set-key (kbd "C-r") 'helm-resume)

;; turn on the mode ...
(helm-mode 1)

;; projectile integration
;(projectile-mode 1)
;(helm-projectile-on)






;;
;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
;;
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(if (display-graphic-p)
    (progn
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))
  (progn
    (global-set-key (kbd "M-[ [ >") 'mc/mark-next-like-this)
    (global-set-key (kbd "M-[ [ <") 'mc/mark-previous-like-this))
  )






;;
;; expand region
;;
(require 'expand-region)
(if (display-graphic-p)
    (progn
      (global-set-key (kbd "C-=") 'er/expand-region))
  (progn
    (global-set-key (kbd "M-[ [ e") 'er/expand-region))
  )
;; After expand-region, region-marking doesn't work anymore (i.e., C-SPC)
;; fix?: https://github.com/magnars/expand-region.el/issues/220
;(setq shift-select-mode nil)






;;
;; elpy: python environment
;; source: https://github.com/jorgenschaefer/elpy
;;
(if (display-graphic-p)
    (progn
      (elpy-enable)))
(setq python-indent 4)

;; This would be required for pylint!
;(setq exec-path (append exec-path '("/Users/samuel/Library/Python/3.6/bin"))) ;; for pylint





;;
;; Snippets (only for coding modes)
;;
(yas-reload-all)
;(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)







;; NeoTree (file navigation)
;; https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p
;; https://github.com/jaypei/emacs-neotree
;; https://github.com/domtronn/all-the-icons.el
;(require 'all-the-icons)
;(require 'neotree)
;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;(defun neotree-project-dir ()
;  "Open NeoTree using the git root."
;  (interactive)
;  (let ((project-dir (ffip-project-root))
;    (file-name (buffer-file-name)))
;    (if project-dir
;    (progn
;      (neotree-dir project-dir)
;      (neotree-find file-name))
;      (message "Could not find git project root."))))
;(provide 'bl-fns)
;;(global-set-key [f8] 'neotree-project-dir)
;(global-set-key (kbd "C-c n") 'neotree-project-dir)






;;
;; cap'n proto mode
;; source: https://github.com/sandstorm-io/capnproto/tree/master/highlighting/emacs
;;
(add-to-list 'load-path "~/.emacs.d/vendor/capnp-mode")
(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))






;;
;; UI customization
;;

;; load a theme
(load-theme 'darktooth t)
;;(load-theme 'monokai t)
;;(set-face-attribute 'default nil :family "Menlo")

;; set a better font-size
(set-face-attribute 'default nil :height 160)

;;
;; powerline -> Is there a better version now in Emacs 26.02?
;; https://github.com/milkypostman/powerline
;;
;;(if (display-graphic-p)
;;    (progn
;;      (add-to-list 'load-path "~/.emacs.d/vendor/powerline")
;;      (require 'powerline)
;;      (powerline-default-theme)
;;      (setq powerline-default-separator 'curve)
;;      (custom-set-faces
;;       ;; custom-set-faces was added by Custom.
;;       ;; If you edit it by hand, you could mess it up, so be careful.
;;       ;; Your init file should contain only one such instance.
;;       ;; If there is more than one, they won't work right.
;;       '(powerline-active1 ((t (:foreground "#444444" :background "#2b73d8" :box nil))))
;;       '(powerline-active2 ((t (:foreground "#545049" :background "#ddd9d2" :box nil))))
;;       '(powerline-inactive1 ((t (:foreground "#666666" :background "#ffa826" :box nil))))
;;       '(powerline-inactive2 ((t (:foreground "#bca0bb" :background "#ddd9d2" :box nil)))))
;;      ;; proper display of powerline on Mac
;;      ;; source: https://emacs.stackexchange.com/questions/14984/emacs-powerline-inconsistent-colors-behind-arrows
;;      (setq ns-use-srgb-colorspace nil)
;;      ))
