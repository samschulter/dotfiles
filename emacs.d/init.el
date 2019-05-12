(defun load-user-file (file) (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file "~/.emacs.d")))


;; Magit
;; Powerline?


(load-user-file "usr/packages.el")
(load-user-file "usr/init-editing.el")
(load-user-file "usr/init-org.el")
(if (display-graphic-p)
    (progn
      (load-user-file "usr/init-auctex.el")))
