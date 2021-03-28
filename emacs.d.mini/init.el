(defun load-user-file (file) (interactive "f")
       "Load a file in current user's configuration directory"
       (load-file (expand-file-name file "~/.emacs.d")))

(load-user-file "usr/packages.el")
(load-user-file "usr/init-basics.el")
(load-user-file "usr/init-editing.el")
