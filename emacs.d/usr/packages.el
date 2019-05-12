(require 'package)

;; package sources
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)


;;; packages to install
;; x ace-window                           ... quickly switch windows. https://github.com/abo-abo/ace-window
;; x auctex                               ... LaTeX environment
;;   dash                                 ... ??? https://github.com/magnars/dash.el
;; x expand-region                        ... text highlighting magic via C-=
;; x flycheck                             ... spell checker
;; x helm                                 ... navigation magic
;; x helm-descbinds                       ... navigation for binded keys: https://github.com/emacs-helm/helm-descbinds
;; x htmlize                              ... export to HTML in org-mode
;; x json-mode
;;   lua-mode
;; x markdown-mode
;; x multiple-cursors                     ... get multiple cursors
;; x rfringe                              ... indicators on displace (e.g., spelling errors)
;; x smooth-scroll                        ... smooth scrolling
;; x org                                  ... magic org-mode
;; x org-bullets                          ... cool bullet points
;; x popup                                ... visual popup menu (required by elpy I guess)
;; x projectile                           ... project management
;; x helm-projectile                      ... helm navigation for project management
;; x protobuf-mode
;; x elpy                                 ... python environment
;; x xclip                                ... copy and paste to X clipboard
;; x yasnippet                            ... snippets
;; x visual-fill-column                   ... wraps lines at fill column, not at the window edge (default)
;; x yaml-mode
;; x zenburn-theme
;; x hc-zenburn-theme
;; x monokai-theme
;; x leuven-theme

(defvar required-packages '(
    ace-window
    auctex
    expand-region
    flycheck
    helm
    helm-descbinds
    htmlize
    json-mode
    markdown-mode
    multiple-cursors
    rfringe
    smooth-scroll
    org
    org-bullets
    popup
    projectile
    helm-projectile
    helm-ag
    protobuf-mode
    elpy
    xclip
    visual-fill-column
    yaml-mode
    zenburn-theme
    hc-zenburn-theme
    monokai-theme
    leuven-theme
    tango-plus-theme
    tangotango-theme
    github-modern-theme
    darktooth-theme
    yasnippet
    yasnippet-snippets
    neotree
    all-the-icons
  )
  "Packages which should be installed upon launch"
)

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
