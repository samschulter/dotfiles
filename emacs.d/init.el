;;
;; Emacs configuration
;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun load-user-file (file) (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file "~/.emacs.d")))

;; emacs needs a path to executables, e.g., ispell or tex
;; https://sdqali.in/blog/2012/05/04/fixing-flyspell-for-emacs-in-mac-os-x/
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
(setq exec-path (append exec-path '("/Library/TeX/texbin")))


(load-user-file "usr/packages.el")
(load-user-file "usr/init-builtin.el")
(load-user-file "usr/init-helm.el")
(load-user-file "usr/init-org.el")
(if (display-graphic-p)
    (progn
      (load-user-file "usr/init-auctex.el")))
(load-user-file "usr/init-edittools.el")
(load-user-file "usr/init-filemodes.el")
(load-user-file "usr/init-ui.el")



;;
;; Resources, documentation, things to try, etc.
;;

;;;; Note for switch between terminal and gui
;(if (display-graphic-p)
;    (progn ;; if graphic
;      (load-user-file "usr/init-auctex.el"))
;  ;; else (optional)
;  (load-user-file "init_terminal.el"))

;;;; confis from others
;; https://github.com/hrs/dotfiles/blob/master/emacs.d/configuration.org
;; https://github.com/aaronbieber/dotfiles/tree/master/configs/emacs.d/lisp
;; http://aaronbedra.com/emacs.d/
;; https://github.com/magnars/.emacs.d

;;;; blogs, etc.
;; https://github.com/xiaohanyu/oh-my-emacs
;; https://www.masteringemacs.org/article/effective-editing-movement
;; https://www.youtube.com/watch?v=JWD1Fpdd4Pc
;; http://pragmaticemacs.com/
;; https://blog.aaronbieber.com/
;; http://emacs.sexy/#resources
;; https://github.com/emacs-tw/awesome-emacs

;;;; Org mode:
;; short guide: http://orgmode.org/orgguide.pdf
;; More tutorials: http://orgmode.org/worg/org-tutorials/
;; http://orgmode.org/orgcard.pdf (the cheatsheet)

;;;; Fixes
;; colors in GNU screen: https://stackoverflow.com/questions/11148321/how-to-preserve-emacs-colors-from-regular-terminal-to-gnu-screen

;;;; Packages to try
;; http://pragmaticemacs.com/emacs/easily-manage-emacs-workspaces-with-eyebrowse/
;; highlight-changes-mode
;; http://emacs-fu.blogspot.com/2009/05/tracking-changes.html (we might only want to highlight the background color a bit?)
;; https://www.emacswiki.org/emacs/TrackChanges (we might want to remove the highlighting after saving the file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (leuven-theme zenburn-theme yaml-mode xclip visual-fill-column smooth-scroll rfringe protobuf-mode org-bullets multiple-cursors monokai-theme markdown-mode json-mode htmlize helm-projectile helm-descbinds hc-zenburn-theme flycheck expand-region elpy auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:foreground "#444444" :background "#a37af5" :box nil))))
 '(powerline-active2 ((t (:foreground "#545049" :background "#ddd9d2" :box nil))))
 '(powerline-inactive1 ((t (:foreground "#666666" :background "#ffa826" :box nil))))
 '(powerline-inactive2 ((t (:foreground "#bca0bb" :background "#ddd9d2" :box nil)))))
