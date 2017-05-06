(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(defvar required-packages '(
    avy
    auctex
    color-theme
    dash
    evil
    evil-leader
    evil-nerd-commenter
    evil-numbers
    evil-surround
    flycheck
    free-keys
    git-messenger
    go-mode
    helm
    helm-descbinds
    htmlize
    json-mode
    lua-mode
    ;;magit
    markdown-mode
    multiple-cursors
    rfringe
    smooth-scroll
    org
    org-bullets
    popup
    ;;powerline
    projectile
    helm-projectile
    protobuf-mode
    ;;python-mode
    elpy
    xclip
    yasnippet
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
