;; Save customizations in this file rather than messing up my init.el file.
(setq custom-file "~/.emacs.d/custom.el")

;; define the packages and themes that should actually be installed
(defvar required-packages '(
    expand-region               ;; Text marker magic via 'C-=': https://github.com/magnars/expand-region.el
    helm                        ;; Navigation magic: https://github.com/emacs-helm/helm
    json-mode                   ;; https://github.com/joshwnj/json-mode
    magit                       ;; Git magic: https://magit.vc
    markdown-mode               ;; https://jblevins.org/projects/markdown-mode/
    multiple-cursors            ;; https://github.com/magnars/multiple-cursors.el
    org                         ;; Note taking magic
    yaml-mode                   ;; https://github.com/yoshiki/yaml-mode
    yasnippet                   ;; Template snippets: https://github.com/joaotavora/yasnippet
    yasnippet-snippets          ;; A collection of many pre-defined snippets: https://github.com/AndreaCrotti/yasnippet-snippets
  )
  "Packages which should be installed upon launch"
)

;; actually install the packages
(dolist (p required-packages)
  (straight-use-package p))
