;; ===========================================================
;; ===========================================================
;; Docs
;; ===========================================================
;; ===========================================================


;; TODO: 2 spaces as indent default for python
;; TODO: 80 lines warping for text, org and latex; (do I want this?)


;; RESOURCES
;; configs of others:
;; https://github.com/hrs/dotfiles/blob/master/emacs.d/configuration.org
;; https://github.com/aaronbieber/dotfiles/tree/master/configs/emacs.d/lisp
;; http://aaronbedra.com/emacs.d/

;; helpful resources:
;; https://github.com/xiaohanyu/oh-my-emacs
;; https://www.masteringemacs.org/article/effective-editing-movement
;; https://www.youtube.com/watch?v=JWD1Fpdd4Pc
;; http://pragmaticemacs.com/

;; (short guide: http://orgmode.org/orgguide.pdf)
;; More tutorials: http://orgmode.org/worg/org-tutorials/
;; http://orgmode.org/orgcard.pdf (the cheatsheet)



;; ===========================================================
;; ===========================================================
;; Packages init, etc.
;; ===========================================================
;; ===========================================================
; (package-initialize)

(defun load-user-file (file) (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file "~/.emacs.d")))

;; install packages
(load-user-file "packages.el")

;; emacs needs a path to executables, e.g., ispell or tex
;; https://sdqali.in/blog/2012/05/04/fixing-flyspell-for-emacs-in-mac-os-x/
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
(setq exec-path (append exec-path '("/Library/TeX/texbin")))





;; ===========================================================
;; ===========================================================
;; Built-in settings
;; ===========================================================
;; ===========================================================

;; emacs built-in settings
(load-user-file "init-builtin.el")




;; ===========================================================
;; ===========================================================
;; UI customization
;; ===========================================================
;; ===========================================================

;; Zenburn
;; (load-theme 'zenburn t)
;; (set-face-attribute 'default nil :height 160)

;; High-contrast Zenburn
;; source: https://github.com/edran/hc-zenburn-emacs
(load-theme 'hc-zenburn t)
(set-face-attribute 'default nil :height 160)

;; Monokai
;; (load-theme 'monokai t)
;; (set-face-attribute 'default nil :family "Menlo")
;; (set-face-attribute 'default nil :height 150)






;; ===========================================================
;; ===========================================================
;; External packages
;; ===========================================================
;; ===========================================================

;; xclip for macOS (xterm)
(load-user-file "clipboard.el")

;; LaTeX
(load-user-file "init-auctex.el")

;; Evil mode (TODO: have a look at it)
;;(load-user-file "init-evil.el")

;; automatic spell checking, requires ispell to be installed
(load-user-file "init-flyspell.el")

;; org mode
(load-user-file "init-org.el")

;; helm:incremental completion
(load-user-file "init-helm.el")

;; Git interface
;;(require 'magit)
;;(global-set-key (kbd "C-x C-g C-s") 'magit-status)
;;(setq magit-push-always-verify nil)
;;(setq magit-last-seen-setup-instructions "1.4.0")

;; remembers last edit point when re-opening a file:
;; https://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el") )
(setq-default save-place t)

;; snippet module
;;(require 'yasnippet)
;;(setq yas-snippet-dirs
;;      '("~/.emacs.d/snippets/yasnippet-snippets"
;;        "~/.emacs.d/snippets/yasmate/snippets"
;;        "~/.emacs.d/snippets/custom"
;;        ))
;;(yas-global-mode 1)

;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; powerline
;; https://github.com/milkypostman/powerline
(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'curve)
(custom-set-faces
 '(powerline-active1 ((t (:foreground "#444444" :background "#94bf7e" :box nil))))
 '(powerline-inactive1 ((t (:foreground "#666666" :background "#d6ad4d" :box nil))))
 '(powerline-active2 ((t (:foreground "#e0bc8d" :background "#ddd9d2" :box nil))))
 '(powerline-inactive2 ((t (:foreground "#bca0bb" :background "#ddd9d2" :box nil)))))
;; proper display of powerline on Mac
;; source: https://emacs.stackexchange.com/questions/14984/emacs-powerline-inconsistent-colors-behind-arrows
(setq ns-use-srgb-colorspace nil)

;; cap'n proto mode
;; source: https://github.com/sandstorm-io/capnproto/tree/master/highlighting/emacs
(add-to-list 'load-path "~/.emacs.d/vendor/capnp-mode")
(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))




;; ===========================================================
;; ===========================================================
;; Coding stuff
;; ===========================================================
;; ===========================================================

;;(add-hook 'python-mode-hook (lambda() (
;;  (setq-default tab-width 2)(setq sh-basic-offset 2)(setq sh-indentation 2))))
;;(setq python-indent-offset 2)
;;(defun python-custom-settings ()
;;  (setq tab-width 2))
;;(add-hook 'python-mode-hook 'python-custom-settings)
(setq python-indent 2)


;; elpy: python environment
;; source: https://github.com/jorgenschaefer/elpy
;; TODO: install code completion stuff (jedi)
;; TODO: how do I change the indent to 2???
(elpy-enable)


;; TODO: with the elpy features C-c C-o, the function below becomes obsolete!
;; code folding w/o any package
;; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
(global-set-key (kbd "<f5>") 'set-selective-display-dlw)
(defun set-selective-display-dlw (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))






