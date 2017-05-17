;; ===========================================================
;; ===========================================================
;; Docs
;; ===========================================================
;; ===========================================================


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
;; https://blog.aaronbieber.com/

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

;; helm
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-mode 1)
;; projectile integration
(projectile-mode 1)
(helm-projectile-on)

;; org-mode
(load-user-file "init-org.el")

;; LaTeX
(load-user-file "init-auctex.el")

;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; spell checker - requires ispell to be installed
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; powerline
;; https://github.com/milkypostman/powerline
(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'curve)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:foreground "#444444" :background "#94bf7e" :box nil))))
 '(powerline-active2 ((t (:foreground "#e0bc8d" :background "#ddd9d2" :box nil))))
 '(powerline-inactive1 ((t (:foreground "#666666" :background "#d6ad4d" :box nil))))
 '(powerline-inactive2 ((t (:foreground "#bca0bb" :background "#ddd9d2" :box nil)))))
;; proper display of powerline on Mac
;; source: https://emacs.stackexchange.com/questions/14984/emacs-powerline-inconsistent-colors-behind-arrows
(setq ns-use-srgb-colorspace nil)

;; cap'n proto mode
;; source: https://github.com/sandstorm-io/capnproto/tree/master/highlighting/emacs
(add-to-list 'load-path "~/.emacs.d/vendor/capnp-mode")
(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

;; remembers last edit point when re-opening a file:
;; https://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace-history") )
(setq-default save-place t)

;; snippet module
;;(require 'yasnippet)
;;(setq yas-snippet-dirs
;;      '("~/.emacs.d/snippets/yasnippet-snippets"
;;        "~/.emacs.d/snippets/yasmate/snippets"
;;        "~/.emacs.d/snippets/custom"
;;        ))
;;(yas-global-mode 1)




;; ===========================================================
;; ===========================================================
;; Coding stuff
;; ===========================================================
;; ===========================================================


;; elpy: python environment
;; source: https://github.com/jorgenschaefer/elpy
;; TODO: install code completion stuff (jedi)
;; TODO: how do I change the default indent to 2???
(elpy-enable)
(setq python-indent 2)

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






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (visual-fill-column zenburn-theme yaml-mode xclip smooth-scroll rfringe protobuf-mode org-bullets multiple-cursors monokai-theme markdown-mode lua-mode json-mode htmlize helm-projectile helm-descbinds hc-zenburn-theme free-keys flycheck evil-surround evil-numbers evil-nerd-commenter evil-leader elpy auctex))))
