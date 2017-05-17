(require 'package)

;; package sources
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; packages to install
(defvar required-packages '(
    auctex
    dash
    flycheck
    free-keys
    helm
    helm-descbinds
    htmlize
    json-mode
    lua-mode
    markdown-mode
    multiple-cursors
    rfringe
    smooth-scroll
    org
    org-bullets
    popup
    projectile
    helm-projectile
    protobuf-mode
    elpy
    xclip
    yasnippet
    visual-fill-column
    yaml-mode
    zenburn-theme
    hc-zenburn-theme
    monokai-theme
  )
  "Packages which should be installed upon launch"
)

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))
