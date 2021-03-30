;; Package manager: Straight (https://github.com/raxod502/straight.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


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
