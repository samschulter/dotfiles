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


;;
;; helm-ag (silver-surfer in emacs)
;; https://github.com/syohex/emacs-helm-ag
(custom-set-variables
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'symbol))
(global-set-key (kbd "C-x C-a") 'helm-do-ag)


;;
;; projectile (integrated with helm)
;;
(projectile-mode 1)
(helm-projectile-on)


;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)


;;
;; spell checker - requires ispell to be installed
;;
(setq exec-path (append exec-path '("/usr/local/bin")))
;; emacs needs a path to executable 'ispell'
;; https://sdqali.in/blog/2012/05/04/fixing-flyspell-for-emacs-in-mac-os-x/
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)


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


;;
;; cap'n proto mode
;; source: https://github.com/sandstorm-io/capnproto/tree/master/highlighting/emacs
;;
(add-to-list 'load-path "~/.emacs.d/vendor/capnp-mode")
(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))


;; blacken - Python formatting
;;
;; https://github.com/python/black
;; blacken: https://github.com/proofit404/blacken/blob/master/blacken.el
;; Installation: 'Add blacken.el to your load-path.'
(add-to-list 'load-path "~/opt/blacken")
(require 'blacken)
;; make sure you have 'black' installed: pip install black
;; To run 'black' automatically before saving, add this:
(add-hook 'python-mode-hook 'blacken-mode)


;; highlight indentation
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;;(add-to-list 'load-path "~/.emacs.d/vendor")
;;;(add-hook 'python-mode-hook 'highlight-indentation-mode)
;;(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)


