(require 'package)

;; Save customizations in this file rather than messing up my init.el file.
(setq custom-file "~/.emacs.d/custom.el")

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; define the packages and themes that should actually be installed
(defvar required-packages '(
    all-the-icons               ;; Icons for emacs ... don't forget to run 'M-x all-the-icons-install-fonts'
    auto-complete               ;; https://github.com/auto-complete/auto-complete
    auctex                      ;; LaTeX
    doom-themes                 ;; collection of multiple themes: https://github.com/hlissner/emacs-doom-themes
    doom-modeline               ;; power line for doom-themes: https://github.com/seagle0128/doom-modeline
    elpy                        ;; Python environment: https://github.com/jorgenschaefer/elpy
    expand-region               ;; Text marker magic via 'C-=': https://github.com/magnars/expand-region.el
    flycheck                    ;; Spell checker
    helm                        ;; Navigation magic: https://github.com/emacs-helm/helm
    helm-ag                     ;; Silver searcher integration into helm
    helm-bibtex                 ;; https://github.com/tmalsburg/helm-bibtex
    helm-projectile             ;; https://github.com/bbatsov/helm-projectile
    htmlize                     ;; Convert buffer content to HTML: https://github.com/hniksic/emacs-htmlize
    json-mode                   ;; https://github.com/joshwnj/json-mode
    magit                       ;; Git magic: https://magit.vc
    markdown-mode               ;; https://jblevins.org/projects/markdown-mode/
    markdown-preview-mode       ;; Preview markdown in the browser: https://github.com/ancane/markdown-preview-mode
    multiple-cursors            ;; https://github.com/magnars/multiple-cursors.el
    org                         ;; Note taking magic
    org-journal                 ;; https://github.com/bastibe/org-journal
    ox-gfm                      ;; Github-flavored markdown exporter for org-mode: https://github.com/larstvei/ox-gfm
    projectile                  ;; Project management: https://github.com/bbatsov/projectile
    yaml-mode                   ;; https://github.com/yoshiki/yaml-mode
    yasnippet                   ;; Template snippets: https://github.com/joaotavora/yasnippet
    yasnippet-snippets          ;; A collection of many pre-defined snippets: https://github.com/AndreaCrotti/yasnippet-snippets
  )
  "Packages which should be installed upon launch"
)

;; actually install the packages
(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;; List of past packages ...
;; - anaconda-mode
;; - monokai-theme
;; - github-modern-theme
;; - darktooth-theme
;; - doom-themes                          ... collecion of multiple domains
;; - py-autopep8
;; - todoist
