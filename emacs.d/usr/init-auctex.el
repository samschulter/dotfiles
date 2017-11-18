;;
;; LaTeX environment
;;

;; Reference: http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
;; NOTE: latex compiling and pdf viewing will only work on my local machine ...

;; enable parsing the tex files on load and save
(setq TeX-parse-self t)
(setq TeX-auto-save t)

;; auctex asks for the master file if info not already
;; added to bottom of file
(setq-default TeX-master nil)

;; activate spell-checking
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; activate visual-line-mode and visual-fill-column-mode
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'visual-fill-column-mode)

;; activate RefTeX
;; It's awesome: https://www.gnu.org/software/emacs/manual/html_node/reftex/RefTeX-in-a-Nutshell.html
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; active RefTex-AucTeX interface
;; see: https://www.gnu.org/software/emacs/manual/html_node/reftex/AUCTeX_002dRefTeX-Interface.html
(setq reftex-plug-into-AUCTeX t)

;; compile tex files to PDFs by default
(setq TeX-PDF-mode t)

;; Use Skim as viewer, enables source <-> PDF sync (!)
;; makes latexmk available via C-c C-c
;; NOTE: SyncTeX is setup via ~/.latexmkrc (!)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
;; NOTE: activate auto-reload in Skim (under Properties/Sync)
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
