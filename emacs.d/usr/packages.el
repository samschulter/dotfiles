(require 'package)

;; Save customizations in this file rather than messing up my init.el file.
(setq custom-file "~/.emacs.d/custom.el")

;; package sources
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; list of potential (and past) packages
;; x all-the-icons                        ... Icons for emacs ... don't forget to run 'M-x all-the-icons-install-fonts'
;; x auctex                               ... LaTeX environment
;; x doom-modeline                        ... A nice modline
;; x elpy                                 ... python environment
;; x expand-region                        ... text highlighting magic via C-=
;; x flycheck                             ... spell checker
;; x helm                                 ... navigation magic
;; x helm-ag                              ... the silver-surfer within emacs and helm
;; x helm-bibtex                          ... use helm to search bibtex files (https://github.com/tmalsburg/helm-bibtex)
;; x helm-projectile                      ... projectile integration into helm
;; x htmlize                              ... export to HTML in org-mode
;; x json-mode
;; x magit                                ... Git magic ...
;; x markdown-mode                        ... https://github.com/defunkt/markdown-mode
;; x markdown-preview-mode                ... https://github.com/ancane/markdown-preview-mode
;; x multiple-cursors                     ... get multiple cursors
;; x org                                  ... magic org-mode
;; x ox-gfm                               ... github flavored markdown exporter for org mode (https://github.com/larstvei/ox-gfm)
;; x projectile                           ... project management within emacs
;; x todoist                              ... TODO list from todoist.com
;; x yaml-mode
;; x yasnippet                            ... snippets for X (e.g., python)
;; x yasnippet-snippets                   ... collection of pre-defined snippets

;; list of potential (and past) themes
;;   monokai-theme
;;   github-modern-theme
;;   darktooth-theme
;;   doom-themes                          ... collecion of multiple domains

;; define the packages and themes that should actually be installed
(defvar required-packages '(
    all-the-icons
    auctex
    doom-modeline
    elpy
    expand-region
    flycheck
    helm
    helm-ag
    helm-bibtex
    helm-projectile
    htmlize
    json-mode
    magit
    markdown-mode
    markdown-preview-mode
    multiple-cursors
    org
    ox-gfm
    projectile
    todoist
    yaml-mode
    yasnippet
    yasnippet-snippets
    doom-themes
  )
  "Packages which should be installed upon launch"
)

;; actually install the packages
(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
