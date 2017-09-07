;; ===========================================================
;; ===========================================================
;; Docs
;; ===========================================================
;; ===========================================================



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
;;(load-theme 'hc-zenburn t)
;;(set-face-attribute 'default nil :height 160)

;; Monokai
;;(load-theme 'monokai t)
;;(set-face-attribute 'default nil :family "Menlo")
;;(set-face-attribute 'default nil :height 160)

;; Wombat
;;(load-theme 'wombat t)
(set-face-attribute 'default nil :height 160)



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

;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; After expand-region, region-marking doesn't work anymore (i.e., C-SPC)
;; fix?: https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)





;; ===========================================================
;; ===========================================================
;; Coding stuff
;; ===========================================================
;; ===========================================================


(setq python-indent 2)

