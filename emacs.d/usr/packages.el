(require 'package)

(setq custom-file "~/.emacs.d/custom.el")

;;; package sources
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

;;; list of potential (and past) packages
;;   ace-window                           ... quickly switch windows. https://github.com/abo-abo/ace-window
;; x auctex                               ... LaTeX environment
;; x elpy                                 ... python environment
;; x expand-region                        ... text highlighting magic via C-=
;; x flycheck                             ... spell checker
;; x helm                                 ... navigation magic
;;   helm-descbinds                       ... navigation for binded keys: https://github.com/emacs-helm/helm-descbinds
;;   helm-projectile                      ... helm navigation for project management
;; x htmlize                              ... export to HTML in org-mode
;; x json-mode
;; x markdown-mode
;; x multiple-cursors                     ... get multiple cursors
;;   neotree
;; x org                                  ... magic org-mode
;;   org-bullets                          ... cool bullet points
;;   popup                                ... visual popup menu (required by elpy I guess)
;;   projectile                           ... project management
;;   protobuf-mode
;;   rfringe                              ... indicators on displace (e.g., spelling errors)
;;   smooth-scroll                        ... smooth scrolling
;;   visual-fill-column                   ... wraps lines at fill column, not at the window edge (default)
;;   xclip                                ... copy and paste to X clipboard
;; x yaml-mode
;; x yasnippet                            ... snippets
;; x yasnippet-snippets

;;; list of potential (and past) themes
;;   zenburn-theme
;;   c-zenburn-theme
;;   monokai-theme
;;   leuven-theme
;;   tango-plus-theme
;;   tangotango-theme
;;   github-modern-theme
;;   darktooth-theme
;; x darktooth-theme


;;; define the packages and themes that should actually be installed
(defvar required-packages '(
    auctex
    elpy
    expand-region
    flycheck
    helm
    htmlize
    json-mode
    markdown-mode
    multiple-cursors
    org
    yaml-mode
    yasnippet
    yasnippet-snippets
    darktooth-theme
  )
  "Packages which should be installed upon launch"
)

;;; actually install the packages
(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
