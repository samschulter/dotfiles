;; ===========================================================
;; ===========================================================
;; Docs
;; ===========================================================
;; ===========================================================




;; ===========================================================
;; ===========================================================
;; Switch (between terminal and GUI)
;; ===========================================================
;; ===========================================================


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun load-user-file (file) (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file "~/.emacs.d")))

(if (display-graphic-p)
    (progn
      ;; if graphic
      (load-user-file "init_full.el"))
  ;; else (optional)
  (load-user-file "init_terminal.el"))
