(defun load-user-file (file) (interactive "f")
       "Load a file in current user's configuration directory"
       (load-file (expand-file-name file "~/.emacs.d")))

(load-user-file "usr/packages.el")
(load-user-file "usr/init-ui.el")
(load-user-file "usr/init-basics.el")
(load-user-file "usr/init-editing.el")
(load-user-file "usr/init-python.el")
(if (display-graphic-p)
    (progn
      (load-user-file "usr/init-org.el")
      (load-user-file "usr/init-auctex.el")))

;; ???
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
