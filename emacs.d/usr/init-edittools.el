;;
;; text - editing tools (multiple cursors, expand-region, etc.)
;;



;; ==> This has some issues with the elpy mode and code-completion :/
;; show a vertical line at the fill-column
;; https://github.com/alpaker/Fill-Column-Indicator
;; https://www.emacswiki.org/emacs/FillColumnIndicator
;(add-to-list 'load-path "~/.emacs.d/vendor")
;(require 'fill-column-indicator)
;(setq fci-rule-width 1)
;(setq fci-rule-color "darkblue")
;; use 'fci-mode' to toggle the minor mode
;; we add a hook for python code
;(add-hook 'python-mode-hook 'fci-mode)



;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
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


;; expand region
(require 'expand-region)
(if (display-graphic-p)
    (progn
      (global-set-key (kbd "C-=") 'er/expand-region))
  (progn
    (global-set-key (kbd "M-[ [ e") 'er/expand-region))
  )
;; After expand-region, region-marking doesn't work anymore (i.e., C-SPC)
;; fix?: https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)


;; elpy: python environment
;; source: https://github.com/jorgenschaefer/elpy
(if (display-graphic-p)
    (progn
      (elpy-enable)))
(setq python-indent 4)


; (require 'flycheck)
; (global-flycheck-mode)
; /Users/samuel/Library/Python/3.6/bin/pylint


;; Snippets (only for coding modes)
(yas-reload-all)
;(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)


;; NeoTree (file navigation)
;; https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p
;; https://github.com/jaypei/emacs-neotree
;; https://github.com/domtronn/all-the-icons.el
(require 'all-the-icons)
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
    (file-name (buffer-file-name)))
    (if project-dir
    (progn
      (neotree-dir project-dir)
      (neotree-find file-name))
      (message "Could not find git project root."))))

(provide 'bl-fns)

;;(global-set-key [f8] 'neotree-project-dir)
(global-set-key (kbd "C-c n") 'neotree-project-dir)
