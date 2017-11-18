;; ===========================================================
;; ===========================================================
;; Docs
;; ===========================================================
;; ===========================================================

;; What's not working for "emacs -nw" out of the box in iterm2
;; on the mac:
;; - expand-region
;; - multiple-cursors
;; - init_auctex.el

;; expand-region did not work because the terminal did not forward
;; some keystrokes, see: https://superuser.com/questions/731427/how-do-i-send-ctrl-in-iterm2
;; Solution: iterm2->keys->send esc seq.: ESC+[ [ e and use this key in emacs.

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
;;(load-theme 'monokai t)
;;(set-face-attribute 'default nil :family "Menlo")
;;(set-face-attribute 'default nil :height 160)

;; Wombat
;;(load-theme 'wombat t)
;;(set-face-attribute 'default nil :height 160)



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
(global-set-key (kbd "C-x C-x") 'helm-all-mark-rings)
;; lets replace isearch-backward with helm-resume (more often used)
(global-set-key (kbd "C-r") 'helm-resume)
;; turn it on
(helm-mode 1)
;; projectile integration
(projectile-mode 1)
(helm-projectile-on)

;; org-mode
(load-user-file "init-org.el")

;; LaTeX
;;(load-user-file "init-auctex.el")

;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines) ;; what is this for?
;;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "M-[ [ >") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-[ [ <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand region
(require 'expand-region)
;;(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-[ [ e") 'er/expand-region)
;; After expand-region, region-marking doesn't work anymore (i.e., C-SPC)
;; fix?: https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

;; spell checker - requires ispell to be installed
;;(dolist (hook '(text-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode 1))))

;; powerline
;; https://github.com/milkypostman/powerline
;;(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
;;(require 'powerline)
;;(powerline-default-theme)
;;(setq powerline-default-separator 'curve)
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(powerline-active1 ((t (:foreground "#444444" :background "#a37af5" :box nil))))
;; '(powerline-active2 ((t (:foreground "#545049" :background "#ddd9d2" :box nil))))
;; '(powerline-inactive1 ((t (:foreground "#666666" :background "#ffa826" :box nil))))
;; '(powerline-inactive2 ((t (:foreground "#bca0bb" :background "#ddd9d2" :box nil)))))
;;;; proper display of powerline on Mac
;;;; source: https://emacs.stackexchange.com/questions/14984/emacs-powerline-inconsistent-colors-behind-arrows
;;(setq ns-use-srgb-colorspace nil)

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



;; ===========================================================
;; ===========================================================
;; Coding stuff
;; ===========================================================
;; ===========================================================


;; elpy: python environment
;; source: https://github.com/jorgenschaefer/elpy
;;(elpy-enable)
;; TODO: can we enable elpy w/o the code-popup menu?
(setq python-indent 2)

