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
;;(setq shift-select-mode nil)


;;
;; Snippets (only for coding modes)
;;
(require 'yasnippet)
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


;;
;; highlight indentation
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;;
;;(add-to-list 'load-path "~/.emacs.d/vendor")
;;;(add-hook 'python-mode-hook 'highlight-indentation-mode)
;;(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)


;;
;; markdown preview mode
;;   use also multimarkdown (https://fletcherpenney.net/multimarkdown/)
;;   for better rendering as described here:
;;   https://github.com/ancane/markdown-preview-mode/issues/1#issuecomment-88168406
;;
(require 'markdown-preview-mode)
;; Set style to github: https://github.com/ancane/markdown-preview-mode/issues/29
(setq markdown-preview-stylesheets
      (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
            "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
(setq markdown-preview-javascript
      (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
"))
(setq markdown-command "multimarkdown")
