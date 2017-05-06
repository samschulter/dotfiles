;; original config from B. Amos
;;(setq-default TeX-PDF-mode t)
;;(setq TeX-auto-save t)
;;(setq TeX-parse-self t)
;;
;;(if (eq system-type 'darwin)
;;  (progn
;;    (setq TeX-view-program-list '(("open" "open %o")))
;;    (setq TeX-view-program-selection '((output-pdf "open")))
;;    (setenv "PATH" (concat (getenv "PATH") "/Library/TeX/texbin:/usr/local/bin"))
;;    (setq exec-path (append exec-path '("/Library/TeX/texbin" "/usr/local/bin"))))
;;  (progn
;;    (setq TeX-view-program-list '(("zathura" "zathura %o")))
;;    (setq TeX-view-program-selection '((output-pdf "zathura")))))

;; config from: http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync (!!!)
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (!!!)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
