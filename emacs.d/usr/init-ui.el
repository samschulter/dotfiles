;;
;; Overall theme
;;
;;(load-theme 'darktooth t)
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;;(load-theme 'doom-one t)
(load-theme 'doom-molokai t)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


;; set a better font-size
(set-face-attribute 'default nil :height 160)


(require 'all-the-icons)
;; Don't forget to run 'M-x all-the-icons-install-fonts'

(require 'doom-modeline)
(doom-modeline-mode 1)

;;;; powerline -> Is there a better version now in Emacs 26.02?
;;;; https://github.com/milkypostman/powerline
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
