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
;;(load-theme 'doom-molokai t)
(load-theme 'doom-opera-light t)

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
(setq doom-modeline-buffer-file-name-style 'buffer-name)
(setq column-number-mode t)
