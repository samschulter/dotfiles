;; We should rename this folder to emacs.d.29.eglot and the other one to ...lsp
;; ...
;; Then, we can put a note here that says we upgraded the previous vertico config
;; to Emacs 29 with lsp first, and then, based on that, we moved to eglot.
;;
;; UPDATES:
;; - Added Markdown & YAML modes. We use those!
;; - Fixes for magit (add package-refresh-contents and :ensure t)
;; - Fixes in company-box (add :ensure t)


;; https://www.adventuresinwhy.com/post/eglot/

;; Corfu instead of company-box?
;; https://github.com/minad/corfu


;; eglot functions!:
;; https://github.com/joaotavora/eglot
;;
;; This shows an eglot configuration!
;; https://github.com/company-mode/company-mode/discussions/1309
;;
;; Flymake vs. flycheck?
;; https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          EMACS INTERNALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adding MELPA - some of the packages below require MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(use-package bind-key) ;; makes it easier to define key-bindings in init.el

(use-package diminish ;; so we can hide minor modes in the mode line
  :ensure t
  )

;; Have one file to store automatically generated customization, instead of
;; putting them into init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Put all file backups into one directory instead of where the file resides
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-backup-files")))

;; Auto-refresh a buffer if opened file changes outside of Emacs
(global-auto-revert-mode t)

;; Downgrade important questions (yes/no) to (y/n)
(fset 'yes-or-no-p 'y-or-n-p)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          EMACS PERFORMANCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst 1mb 1048576)
(defconst 20mb 20971520)
(defconst 30mb 31457280)
(defconst 50mb 52428800)

(defun fk/defer-garbage-collection ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun fk/restore-garbage-collection ()
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold 30mb))))

(add-hook 'emacs-startup-hook 'fk/restore-garbage-collection)
(add-hook 'minibuffer-setup-hook 'fk/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook 'fk/restore-garbage-collection)

(setq read-process-output-max (* 3 1mb))  ;; lsp-mode's performance suggestion





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          USER INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t) ;; no welcome message after startup
(setq initial-scratch-message nil) ;; start with an empty *scratch* buffer
(setq initial-major-mode 'org-mode) ;; new buffers (& *scratch*) are org mode

;; Window size and position of Emacs after startup
(setq initial-frame-alist '((width . 138) ; chars
                            (height . 42) ; lines
                            (left . 0)
                            (top . 0)))
(setq default-frame-alist '((width . 138)
                            (height . 42)
                            (left . 0)
                            (top . 0)))

;; Remove tool-bar, scroll-bar and menu-bar of Emacs - not needed
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(load-theme 'whiteboard) ;; "whiteboard" is part of original Emacs (elpa)
(set-face-attribute 'default nil ;; increase the font size
                    :height 160)

(setq ring-bell-function 'ignore) ;; no sound
(column-number-mode t) ;; Display column number in modeline

;; Show the (built-in) line-numbers, except in some modes
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                org-journal-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  )


(setq scroll-error-top-bottom t) ;; continue scroll til POINT is at top/bottom
(setq scroll-preserve-screen-position t) ;; point keeps location when scrolling
(setq scroll-conservatively 100)
(pixel-scroll-precision-mode t) ;; New in Emacs 29.0

;; When splitting windows, move point to other window
(bind-key "C-x 2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(bind-key "C-x 3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(bind-key "C-x k" 'kill-current-buffer)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          GENERAL TEXT EDITING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fill-column 80) ;; Auto. line wrapping happens at this column

;; If nil, a single space is sufficient to recognize a sentence's end. This
;; means commands like <M-a> and <M-e> stop at sentences even without double
;; spaces. If non-nil, a sentence's end is only recognized with a double space.
(setq sentence-end-double-space nil)

;; Override selected text when starting to type, instead of appending new text
(delete-selection-mode t)

(setq require-final-newline t) ;; End every file with a newline

(setq-default show-trailing-whitespace t)

(show-paren-mode 1) ;; highlights start/ending parenthesis
(setq show-paren-delay 0) ;; highlight parenthesis immediately
;; Insert the closing bracket automatically when typing the opeining one
(electric-pair-mode 1)

;; Show the minibuffer after 0.5 sec (default: 1), with further key options
(use-package which-key
  :ensure t
  :diminish
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)  ;; show minibuffer after 0.5 sec
  ;; reduce the delay by 0.5 to 0 sec for all subsequent key strokes
  (setq which-key-idle-secondary-delay 0.5)
  )

;; Switch keybord shortcuts of C-a and M-m
(bind-key "C-a" 'back-to-indentation)
(bind-key "M-m" 'move-beginning-of-line)

;; default indentation of 4 spaces
(setq-default indent-tabs-mode nil) ; no TAB for indent
(setq-default tab-width 4)
(setq sh-basic-offset 4)
(setq sh-indentation 4)

;; When viewing a file as symlink, use real file to not sidestep version control
(setq vc-follow-symlinks t)

;; Capital letters in CamelCase stop point in "forward/backword-word" commands
(use-package subword
  :diminish subword-mode
  :hook
  (prog-mode . subword-mode)
  (org-mode . subword-mode)
  )

;; Real helpful: Expand highlight at point within "context", e.g., parenthesis
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region))
  )

;; Real helpful: Multiple cursors for text editing
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this))
  )

(use-package flyspell
  :ensure t
  :diminish (flyspell-mode " Spll")
  :config
  (setq exec-path (append exec-path '("/usr/local/bin")))
  :hook
  (text-mode . flyspell-mode)
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          FILE HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf  ;; Remember recently opened files
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items nil)
  )

(use-package dired
  :custom
  (dired-listing-switches "-lAhp") ;; No '--group-directories-first' in macOS
  (dired-dwim-target t)
  :init
  ;; Enable the command "dired-find-alternate-file" (originally, it's disabled)
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("H" . dired-hide-details-mode))
  )

(use-package dired-subtree
  :ensure t
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)
        ("<tab>" . dired-subtree-toggle))
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          ORG MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :diminish org-indent-mode
  :init (setq org-startup-indented t
              org-src-fontify-natively t
              org-cycle-separator-lines 1
              org-src-tab-acts-natively t
              org-log-into-drawer t
              org-archive-location "%s_archive::datetree/"
              org-todo-keywords '((sequence "TODO" "|" "DONE"))
              org-enforce-todo-dependencies t
              org-log-done (quote time))
  :bind (:map org-mode-map
              ("RET" . org-return-and-maybe-indent)
              ("<C-return>" . org-meta-return))
  :hook ((org-mode . flyspell-mode)
         (org-mode . visual-line-mode))
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          EMACS COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is essentially a better visualization for Emacs' completion system
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-count 20)
  :bind (:map minibuffer-local-map
         ("C-j" . vertico-insert)
         ("C-f" . vertico-insert)
         ("C-l" . vertico-directory-up))
  )

;; Customize sorting for files to not be based on history but alphabet
(use-package vertico-multiform
  :after vertico
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-categories
        '((file (vertico-sort-function . vertico-sort-alpha))))
  )

;; Repeat vertico sessions
(use-package vertico-repeat
  :after vertico
  :bind
  ("C-r" . vertico-repeat)
  ("C-S-r" . vertico-repeat-select)
  )
;; To repeat vertico sessions, we need to save them every time the minibuffer
;; is invoked. This caused me some issues before where Emacs would just become
;; responseless ... so let's keep an eye on that ...
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

;; Make history persistent over Emacs starts - vertico uses history for sorting
(use-package savehist
  :init
  (savehist-mode)
  )

;; This is recommended by vertico
(use-package emacs
  :init
  (setq enable-recursive-minibuffers t)
  )

;; Orderless completion style: "bb aa" finds "aa bb"
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  )

;; Additional information about files, buffers, etc. in minibuffer. This
;; information is called annotators. Use "marginalia-cycle" to cycle through
;; annotator types (like None, builtin, marginalia)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  )


;; Lots of improved versions of existing functions, like switching buffers. The
;; improvements are "live preview" for example. This is so powerful, see more
;; at https://github.com/minad/consult
;; Most of the settings below are from the recommended settings in consult.
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g M-g" . consult-goto-line)
         ("C-s" . consult-line)
         ;; ("C-c c" . hydra-consult/body)
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; This was needed to fix some issues in org mode ... cant' recall what
  ;;(setq org-fold-core-style 'overlays)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   ;; :preview-key '(:debounce 0.4 any)
   consult-line :prompt "Search: "
   )

  ;; Add function to search from point
  (defalias 'consult-line-thing-at-point 'consult-line)
  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol)
   :prompt "Search from point: "
   )
  (bind-key "M-s" 'consult-line-thing-at-point)

  ;; Optionally configure the narrowing key. ??? I don't know how this is useful
  (setq consult-narrow-key "C-+")

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          PYTHON ENVIRONMENT - CODING GENERAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-c ;" 'comment-or-uncomment-region)

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status))
  :hook
  (git-commit-setup . git-commit-turn-on-flyspell)
  )

;; Current version of git-gutter has an issue with Emacs 29

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :defer nil
  :custom
  (yas-indent-line nil)
  (yas-inhibit-overlay-modification-protection t)
  :custom-face
  (yas-field-highlight-face ((t (:inherit region))))
  :hook
  (snippet-mode . (lambda () (setq-local require-final-newline nil)))
  :config
  (yas-global-mode)
  )

;; (use-package yasnippet-snippets
;;   :ensure t
;;   )

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  :defer nil
  )

(use-package conda
  :ensure t
  :init
  (setq conda-env-home-directory (expand-file-name "~/work/opt/miniconda3"))
  (setq conda-anaconda-home (expand-file-name "~/work/opt/miniconda3"))

  ;; Update the modeline to include the current conda environment. (See
  ;; variable "mode-line-format" in general.) It would be better if we can
  ;; check that the list does not already contain the conda environment
  (setq-default global-mode-string (cons '(t (:eval (if conda-env-current-name (format "Conda[%s] " conda-env-current-name)))) global-mode-string))
  )

;; (use-package company
;;   :ensure t
;;   :after eglot
;;   :hook (eglot-managed-mode . company-mode)  ; start company when eglot is started
;;   :diminish (company-mode "Cmplt")
;;   :custom
;;   ;; https://emacs.stackexchange.com/questions/12441/is-it-possible-to-start-company-completion-without-a-prefix
;;   ;; (company-minimum-prefix-length 1)
;;   ;; (company-idle-delay 0.05)
;;   (company-minimum-prefix-length 2)
;;   (company-idle-delay 0.15)
;;   )

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)  ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-no-match 'separator)
  (corfu-auto t)  ;; Enable automatically opening the completion box
  (corfu-auto-delay 0.2)  ;; that's the default
  (corfu-auto-prefix 2)  ;; default here was 3

  ;; TODO: we need to make the completion-styles nclude orderless fast only inside the curfu box! Otherwise consult-line does not work properly anymore!
  ;; (completion-styles '(orderless-fast basic))
  :init
  (global-corfu-mode)  ;; Recommended ...
  ;; :after eglot
  ;; :hook (eglot-managed-mode . corfu-mode)
  )


;; Not quite sure how this is now linked with corfu?! But the package itself is cool I guess
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p t)  ;; not sure if this is really helpful
  )

(use-package eglot)
;; Also need to do treesit-install-language-grammer ... python ... ENTER multiple times
;; Can I do this here in the init.el file too?
;; I also had to run "eglot-upgrade-eglot" and restart to get a "more quiet" version of eglot

;; https://www.reddit.com/r/emacs/comments/14zk34c/eglot_seems_to_use_pyflakes_but_not_flake8/
;; Why not just set: "python-flymake-command"
(setq-default eglot-workspace-configuration
              ;; the correct format is described here: https://joaotavora.github.io/eglot/#Eglot-Variables
              ;; the configuration to setup flake8 here: https://github.com/python-lsp/python-lsp-server#configuration
              '((:pyls . (:plugins (:mccabe (:enabled :json-false))))
                (:pylsp . (:plugins (:mccabe (:enabled :json-false)
                                             :pycodestyle (:enabled :json-false)
                                             :pyflakes (:enabled :json-false)
                                             :flake8 (:enabled t)
                                             :rope_autoimport (:enabled :json-false))
                                    :configurationSources ["flake8"]))))

(use-package python-mode
  :ensure nil  ;; use Emacs built-in version
  :mode (("\\.py\\'" . python-ts-mode))
  :hook
  (python-ts-mode . my/py-activate-conda-then-eglot) ;; Open conda and connect to language server when Python file is opened
  )

(defun my/py-activate-conda-then-eglot ()
  "Activate eglot after activating conda environment."
  (interactive)
  (progn
    (if (not (window-minibuffer-p))
      ;; only run if not in minibuffer - otherwise things get messed up with buffer preview in ivy/counsel mode
      (if conda-env-current-name
          (progn
            (message "CONDA environment is active")
            (eglot-ensure))
        (progn
          (message "No CONDA environment is active ...")
          (call-interactively #'conda-env-activate)
          (eglot-ensure))))))

(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  ;; (set-face-foreground 'highlight-indent-guides-character-face "black")
  (setq highlight-indent-guides-method 'character)
  )

(use-package flymake
  :ensure t
  :config
  ;; Only run flymake syntax checks after 2 seconds of no changes. Otherwise, typing code becomes
  ;; rather slow in my experience ... actually not sure if that's the issue for slowdown
  ;; ??? If we set this to nil, would that trigger flymake to only run at load-file and save-file?
  (setq flymake-no-changes-timeout 2)
  ;;(setq flymake-no-changes-timeout nil)
  )

;; (use-package flycheck
;;   :ensure t
;;   :hook
;;   (flycheck-mode . flymake-mode-off)
;;   (python-mode . flycheck-mode)
;;   :config
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save))  ;; Only check the buffer when it was saved
;;   (setq flycheck-python-flake8-executable (expand-file-name "~/work/opt/miniconda3/bin/flake8"))
;;   (setq-default flycheck-disabled-checkers '(python-pylint python-mypy))
;; )

(use-package yaml-mode
  :ensure t
  )

(use-package markdown-mode  ;; this config is an example from https://jblevins.org/projects/markdown-mode/
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)  ;; gfm = Github-flavored markdown
  :init
  (setq markdown-command "multimarkdown")
)


(use-package hydra
  :ensure t
  ;; :bind
  ;; ("C-c p" . hydra-prog-python/body)
  )

(defhydra hydra-prog-python (:color blue :hint nil) ;; global-map "C-c p"
  "
 ^Navigation^              ^Project^             ^Coding
 ^^---------------------- ^^------------------- ^^-----------------------------
 _l_: goto line            _pb_: switch buffer   _i_:  imenu
 _f_: find files           _pf_: find file       _I_:  imenu-multi
 _F_: focus lines          _pd_: dired           _cr_: rename
 _S_: search multi files   _pr_: recent file     _cf_: find-references
 _g_: grep                 _pi_: ibuffer         _ci_: find-definition
 _G_: git grep             _pg_: grep            _cd_: eldoc
 ^ ^                       _po_: multi occur     _ca_: code actions
 ^ ^                       _pK_: kill buffers
 ^ ^                       ^  ^                  ^Errors
 ^ ^                       ^  ^                 ^^-----------------------------
 ^ ^                       ^  ^                  _e_: flymake diagnostics
"
  ;; Navigation
  ("l" consult-goto-line)
  ("f" consult-find)
  ("F" consult-focus-lines)
  ("S" consult-line-multi)
  ("g" consult-grep)
  ("G" consult-git-grep)

  ;; Project
  ("pb" consult-project-buffer)
  ("pf" projectile-find-file)
  ("pd" projectile-dired)
  ("pr" projectile-recentf)
  ("pi" projectile-ibuffer)
  ("pg" projectile-grep)
  ("po" projectile-multi-occur)
  ("pK" projectile-kill-buffers)

  ;; Coding
  ("i" consult-imenu)
  ("I" consult-imenu-multi)
  ("cr" eglot-rename)
  ("cf" xref-find-references)
  ("ci" xref-find-definitions)
  ("cd" eldoc)
  ("ca" eglot-code-actions)

  ;; Errors
  ("e" flymake-show-buffer-diagnostics)

  ("q" nil "cancel" :color red)
  )

(bind-key "C-c p" 'hydra-prog-python/body)
