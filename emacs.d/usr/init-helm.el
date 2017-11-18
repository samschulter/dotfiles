;;
;; helm - navigation
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

;; turn it on
(helm-mode 1)

;; projectile integration
(projectile-mode 1)
(helm-projectile-on)
