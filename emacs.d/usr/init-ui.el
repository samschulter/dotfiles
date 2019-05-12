;;
;; UI customization
;;

;; (load-theme 'zenburn t)
;; High-contrast Zenburn - https://github.com/edran/hc-zenburn-emacs
;;(load-theme 'hc-zenburn t)
;;(load-theme 'monokai t)
;;(set-face-attribute 'default nil :family "Menlo")
;;(load-theme 'wombat t)
;;(load-theme 'leuven t)
;;(load-theme 'tango-plus t)
;;(load-theme 'tangotango t)
;;(load-theme 'github-modern t)
(load-theme 'darktooth t)

(set-face-attribute 'default nil :height 160)


;; powerline
;; https://github.com/milkypostman/powerline
(if (display-graphic-p)
    (progn
      (add-to-list 'load-path "~/.emacs.d/vendor/powerline")
      (require 'powerline)
      (powerline-default-theme)
      (setq powerline-default-separator 'curve)
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(powerline-active1 ((t (:foreground "#444444" :background "#2b73d8" :box nil))))
       '(powerline-active2 ((t (:foreground "#545049" :background "#ddd9d2" :box nil))))
       '(powerline-inactive1 ((t (:foreground "#666666" :background "#ffa826" :box nil))))
       '(powerline-inactive2 ((t (:foreground "#bca0bb" :background "#ddd9d2" :box nil)))))
      ;; proper display of powerline on Mac
      ;; source: https://emacs.stackexchange.com/questions/14984/emacs-powerline-inconsistent-colors-behind-arrows
      (setq ns-use-srgb-colorspace nil)
      ))

;;'(powerline-active1 ((t (:foreground "#444444" :background "#a37af5" :box nil))))
;;'(powerline-active2 ((t (:foreground "#545049" :background "#ddd9d2" :box nil))))
;;'(powerline-inactive1 ((t (:foreground "#666666" :background "#ffa826" :box nil))))
;;'(powerline-inactive2 ((t (:foreground "#bca0bb" :background "#ddd9d2" :box nil)))))
