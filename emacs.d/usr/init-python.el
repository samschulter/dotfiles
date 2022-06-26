;; https://realpython.com/emacs-the-best-python-editor/#additional-python-language-features


;;
;; Python interpreter
;; FIXME: There must be a better way to automatically get a default virtual env
;;
(if (display-graphic-p)
    (progn
      (setq python-shell-interpreter "~/work/opt/miniconda3/bin/python")))
;;(setq python-shell-interpreter "~/work/opt/anaconda3/bin/python")


;;
;; elpy: python environment
;; source: https://github.com/jorgenschaefer/elpy
;;
(elpy-enable)
;;(setq exec-path (append exec-path '("~/Library/Python/2.7/bin"))) ;; for virutalenv in elpy
(setq exec-path (append exec-path '("~/work/opt/miniconda3/bin/python"))) ;; for virutalenv in elpy


;;
;; Enable Flycheck
;; https://www.flycheck.org/en/latest/index.html
;;
;; Install flake8 via homebrew: 'brew install flake8'
;;
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  ;; Only check the buffer when it was saved
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;;(setq flycheck-python-flake8-executable "flake8"))
  (setq flycheck-python-flake8-executable "/Users/samuel/work/opt/miniconda3/bin/flake8"))

(setq python-indent 4)


;;
;; blacken - Python formatting
;;
;; https://github.com/python/black
;; blacken: https://github.com/proofit404/blacken/blob/master/blacken.el
;; Installation: 'Add blacken.el to your load-path.'
;; (add-to-list 'load-path "~/.emacs.d/vendor/blacken")
;; (require 'blacken)
;; ;; make sure you have 'black' installed: pip install black
;; ;; To run 'black' automatically before saving, add this:
;; (add-hook 'python-mode-hook 'blacken-mode)


;;
;; autopep8
;; https://github.com/paetzke/py-autopep8.el
;; https://github.com/hhatto/autopep8
;;
;;(require 'py-autopep8)
;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;(setq exec-path (append exec-path '("~/work/opt/anaconda3/bin"))) ;; for autopep8





