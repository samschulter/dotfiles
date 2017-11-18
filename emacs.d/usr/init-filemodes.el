;;
;; settings for file-modes - e.g., yaml-mode, json-mode, capnp-mode, etc.
;;


;; cap'n proto mode
;; source: https://github.com/sandstorm-io/capnproto/tree/master/highlighting/emacs
(add-to-list 'load-path "~/.emacs.d/vendor/capnp-mode")
(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

