;; ===========================================================
;; ===========================================================
;; Docs
;; ===========================================================
;; ===========================================================

;; resources:
;; http://emacs.sexy/#resources
;; https://github.com/emacs-tw/awesome-emacs


;; fixes:
;; colors in GNU screen: https://stackoverflow.com/questions/11148321/how-to-preserve-emacs-colors-from-regular-terminal-to-gnu-screen

;; interesting packages:
;; http://pragmaticemacs.com/emacs/easily-manage-emacs-workspaces-with-eyebrowse/


;; Things to try:
;; highlight-changes-mode:
;; http://emacs-fu.blogspot.com/2009/05/tracking-changes.html (we might only want to highlight the background color a bit?)
;; https://www.emacswiki.org/emacs/TrackChanges (we might want to remove the highlighting after saving the file)






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
