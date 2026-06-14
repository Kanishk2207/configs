;;; kanishk-conf.el --- Personal configuration  -*- lexical-binding: t; -*-

;; ====================
;; Package / MELPA
;; ====================
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(eval-when-compile
  (require 'use-package))

;; ====================
;; Emacs 30: use tree-sitter modes
;; ====================
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode     . js-ts-mode)
        (js2-mode    . js-ts-mode)
        (css-mode    . css-ts-mode)
        (c-mode      . c-ts-mode)
        (c++-mode    . c++-ts-mode)
        (go-mode     . go-ts-mode)
        (rust-mode   . rust-ts-mode)
        (elixir-mode . elixir-ts-mode)))

(setq treesit-font-lock-level 4)

;; ==========================
;; Visual tweaks: cursor + region
;; ==========================
(defvar my/cursor-color "white"
  "The only cursor color allowed. Change this var to alter the cursor color.")

(defun my/lock-cursor-color (&rest _)
  "Force the cursor color to stay `my/cursor-color`, ignoring theme changes."
  (set-cursor-color my/cursor-color)
  (set-face-attribute 'cursor nil :background my/cursor-color))

(defun my/set-region-color (&rest _)
  "Force region (selection) to yellow regardless of theme."
  (set-face-attribute 'region nil
                      :background "yellow"
                      :foreground "black"))

;; Apply immediately
(my/lock-cursor-color)
(my/set-region-color)

;; Re-apply whenever a theme loads (daemon-safe)
(advice-add 'load-theme :after #'my/lock-cursor-color)
(advice-add 'load-theme :after #'my/set-region-color)

;; Re-apply when new frames are created (daemon-safe)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my/lock-cursor-color)
              (my/set-region-color))))


;; ====================
;; Dired mode settings
;; ====================
(setq insert-directory-program "gls")

(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))

;; ====================
;; GUI Setup (daemon-safe)
;; ====================
(defun my/gui-setup (frame)
  (with-selected-frame frame
    ;; Cursor shape
    (setq-default cursor-type 'bar)
    ;; Region color is already handled by my/set-region-color advice

    ;; Scrollbar / fringe tweaks (frame-local)
    (modify-frame-parameters frame '((scroll-bar-width . 4)))
    (fringe-mode 4)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/gui-setup)
  (my/gui-setup (selected-frame)))

;; ====================
;; LSP Mode
;; ====================
(use-package lsp-mode
  :ensure t
  :init
  ;; Corfu + CAPF setup (avoid company-mode autoconfig warnings).
  (setq lsp-completion-provider :capf)
  ;; Silence snippet warning when yasnippet isn't installed.
  (setq lsp-enable-snippet nil)
  ;; (setq lsp-semantic-tokens-enable t)   ;; enable semantic tokens
  :hook
  ((python-mode    . lsp)
   (python-ts-mode . lsp)
   (go-mode        . lsp)
   (go-ts-mode     . lsp)
   (js-mode        . lsp)
   (js-ts-mode     . lsp)
   (rust-mode      . lsp)
   (rust-ts-mode   . lsp))
  :commands lsp)

;; LSP UI
(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  :hook
  (lsp-mode . lsp-ui-mode))

;; Refresh semantic tokens after theme changes for all LSP buffers
(with-eval-after-load 'lsp-mode
  (defun my/lsp-refresh-semantic-tokens-after-theme (&rest _)
    "Refresh LSP semantic tokens in all buffers after a theme change."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p lsp-mode)
          (ignore-errors
            (lsp-semantic-tokens-refresh))))))
  (advice-add 'load-theme :after #'my/lsp-refresh-semantic-tokens-after-theme))

;; ====================
;; Whitespace Mode
;; ====================
(setq whitespace-style
      '(face trailing space-before-tab empty space-after-tab))
(global-whitespace-mode 1)

(with-eval-after-load 'whitespace
  ;; Trailing spaces → red
  (set-face-attribute 'whitespace-trailing nil
                      :background "red"
                      :foreground nil)

  ;; Empty lines at buffer end → grey
  (set-face-attribute 'whitespace-empty nil
                      :background "grey20"
                      :foreground nil)

  ;; Tabs → subtle grey underline
  (set-face-attribute 'whitespace-tab nil
                      :background nil
                      :underline t
                      :foreground "dim gray")

  (set-face-attribute 'whitespace-indentation nil
                      :background nil
                      :foreground nil))

;; ====================
;; Projectile
;; ====================
(with-eval-after-load 'projectile
  (setq projectile-switch-project-action #'projectile-dired))

;; ====================
;; Keybindings for LSP navigation
;; ====================
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'lsp-ui

    ;; 1. Unbind M-? from minor modes that override it
    (with-eval-after-load 'smartparens
      (define-key smartparens-mode-map (kbd "M-?") nil))
    (with-eval-after-load 'anaconda-mode
      (define-key anaconda-mode-map (kbd "M-?") nil))

    ;; 2. Set up keybindings for LSP navigation
    (global-set-key (kbd "M-.") #'lsp-find-definition)
    (global-set-key (kbd "M-?") #'lsp-ui-peek-find-references)
    (global-set-key (kbd "M-,") #'xref-pop-marker-stack)

    ;; 3. Optional: make M-? local in programming buffers
    (add-hook 'prog-mode-hook
              (lambda ()
                (local-set-key (kbd "M-?") #'lsp-ui-peek-find-references)))))

;; ====================
;; Python Enhancements TODO: add venv restart and along with lsp restart on project switch
;; ====================
(with-eval-after-load 'python
  ;; Jump between blocks at same indentation
  (define-key python-ts-mode-map (kbd "M-n") #'python-nav-forward-block)
  (define-key python-ts-mode-map (kbd "M-p") #'python-nav-backward-block)

  ;; Jump up/down indentation levels
  (define-key python-ts-mode-map (kbd "M-u") #'python-nav-backward-up-list) ;; up to parent
  (define-key python-ts-mode-map (kbd "M-d") #'python-nav-forward-statement)) ;; down into child

;; -----------------------------------------
;; Python LSP Setup (Pyright + Ruff-LSP)
;; -----------------------------------------

;; Detect virtualenv Python
(defun my/lsp-pyright-locate-python-from-pyvenv ()
  "Return Python executable from active pyvenv virtualenv."
  (when (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
    (let ((python (expand-file-name "bin/python" pyvenv-virtual-env)))
      (when (file-executable-p python) python))))

;; Format on save
(defun my/python-lsp-format-on-save ()
  "Format buffer using LSP and organize imports using Pyright."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (lsp-format-buffer)))


(defun my/python-lsp-setup ()
  "Enable auto-format and auto-import on save for Python."
  (add-hook 'before-save-hook #'my/python-lsp-format-on-save nil t))


;; -----------------------------------------
;; Add python-ts-mode support to LSP clients
;; -----------------------------------------
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python")))


;; -----------------------------------------
;; Ruff LSP (formatter & lint server)
;; -----------------------------------------
(with-eval-after-load 'lsp-ruff
  (setq lsp-ruff-major-modes '(python-mode python-ts-mode)))


(defvar my/ruff-allowed-kinds
  '("source.fixAll.ruff"
    "source.organizeImports.ruff")
  "List of Ruff code action kinds that should auto-run on save.")


;; fix auto fixable issues and organises imports
(defun my/ruff-apply-actions ()
  "Apply only approved Ruff code actions on save."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/codeAction"))
    (let ((actions (lsp-request
                    "textDocument/codeAction"
                    (lsp--text-document-code-action-params))))
      (dolist (action actions)
        (let ((kind (gethash "kind" action)))
          (when (member kind my/ruff-allowed-kinds)
            (lsp-execute-code-action action)))))))


(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/ruff-apply-actions nil t)))


(add-hook 'python-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/ruff-apply-actions nil t)))


;; -----------------------------------------
;; Ruff linting (Flymake)
;; -----------------------------------------
(use-package flymake-ruff
  :ensure t
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))


;; -----------------------------------------
;; Pyright (type checking + imports)
;; -----------------------------------------
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :init
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python"))
  :custom
  (lsp-pyright-modes '(python-mode python-ts-mode))
  (lsp-pyright-type-checking-mode "basic")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-auto-detect-venv t)
  :hook ((python-mode . my/python-lsp-setup)
         (python-ts-mode . my/python-lsp-setup))
  :config
  (with-eval-after-load 'pyvenv
    (add-to-list 'lsp-pyright-python-search-functions
                 #'my/lsp-pyright-locate-python-from-pyvenv)))

;; ====================
;; Ediff
;; ====================
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; ====================
;; Swiper
;; ====================
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-s" . swiper-all)))

;; ====================
;; Redo
;; ====================
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; ====================
;; Ace Window
;; ====================
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("s-w"   . ace-window))
  :config
  (setq aw-dispatch-always t)
  (setq aw-scope 'frame))

;; ====================
;; Make scroll bar + fringe consistent
;; ====================
(modify-all-frames-parameters '((scroll-bar-width . 4)))
(fringe-mode 4)

;; ====================
;; Kill ring delete
;; ====================
(defun kill-ring-delete-entry (string)
  "Delete STRING from the kill-ring."
  (interactive
   (list (completing-read "Delete from kill-ring: " kill-ring)))
  (setq kill-ring (delete string kill-ring))
  (message "Deleted: %s" string))

;; ====================
;; change ace window shortcut to C-c o
;; ====================
(define-key prelude-mode-map (kbd "C-c o") nil)
(global-set-key (kbd "C-c o") 'ace-window)
(global-set-key (kbd "C-c C-o") 'ace-window)

;; ====================
;; Multiple cursors
;; ====================
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

;; ====================
;; Flycheck threshold
;; ====================
(with-eval-after-load 'flycheck
  (setq flycheck-checker-error-threshold 2000))

;; ====================
;; Smooth scrolling (pixel-scroll)
;; ====================
(use-package pixel-scroll
  :init
  (pixel-scroll-precision-mode 1)
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-interpolation-factor 0.00005))

;; ====================
;; Font size
;; ====================
(set-face-attribute 'default nil :height 105)

;; ====================
;; Magit window split direction
;; ====================
(with-eval-after-load 'magit
  (setq magit-display-buffer-function #'magit-display-buffer-traditional)
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

;; ---------- Corfu Setup ----------
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preselect 'first)
  (corfu-scroll-margin 4)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-max-width 80)
  (corfu-count 14)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<down>" . corfu-next)
        ("<up>" . corfu-previous)
        ("M-RET" . corfu-insert)
        ("RET" . corfu-insert)
        ("TAB" . corfu-next)
        ("S-TAB" . corfu-previous)))

;; ---------- LSP Integration with Corfu ----------
(add-hook 'lsp-mode-hook #'corfu-mode)

;; Better fuzzy matching for Corfu + LSP
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((lsp-capf (styles orderless)))))

;; ====================
;; Move line up/down with Super + ↑ / ↓
;; ====================
(use-package move-text
  :ensure t
  :config
  (global-set-key (kbd "s-<up>") 'move-text-up)
  (global-set-key (kbd "s-<down>") 'move-text-down))


;; ============================
;; Hl todo settings
;; ============================
(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . hl-todo-mode)))

(setq hl-todo-keyword-faces
      '(("TODO"   warning bold)
        ("FIXME"  error bold)
        ("NOTE"   success bold)
        ("INFO"   font-lock-doc-face bold)
        ("DEBUG"  font-lock-constant-face bold)))



;; ============================
;; Nuclear option: Disable Company completely
;; ============================
;; (setq prelude-company nil)

;; (when (featurep 'company)
;;   (global-company-mode -1)
;;   (unload-feature 'company t))

;; (with-eval-after-load 'company
;;   (global-company-mode -1))

;; (defun my/kill-company-in-buffer ()
;;   "Ensure company-mode is off in current buffer."
;;   (when (bound-and-true-p company-mode)
;;     (company-mode -1)))
;; (add-hook 'after-change-major-mode-hook #'my/kill-company-in-buffer)

;; (dolist (hook '(prog-mode-hook
;;                 python-mode-hook
;;                 emacs-lisp-mode-hook
;;                 go-mode-hook))
;;   (add-hook hook (lambda () (company-mode -1)) 90))

;; (defun my/prevent-company-mode (orig-fun &optional arg &rest args)
;;   "Prevent company-mode from being enabled."
;;   (when (or (not arg) (<= arg 0))
;;     (apply orig-fun arg args)))
;; (advice-add 'company-mode :around #'my/prevent-company-mode)

;; ============================
;; Disable anaconda mode entirely
;; ============================
;; (use-package anaconda-mode
;;   :disabled t)
;; (use-package company-anaconda
;;   :disabled t)

;; (with-eval-after-load 'python
;;   (remove-hook 'python-mode-hook 'anaconda-mode)
;;   (remove-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; (with-eval-after-load 'anaconda-mode
;;   (setq anaconda-mode nil)
;;   (setq anaconda-eldoc-mode nil)
;;   (when (boundp 'anaconda-mode-map)
;;     (setcdr anaconda-mode-map nil)))

;; ============================
;; Auto detect python version
;; ============================
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-tracking-mode 1))

;; ============================
;; Confirm before quitting
;; ============================
(defun confirm-before-quit ()
  "Ask for confirmation before quitting Emacs."
  (interactive)
  (when (yes-or-no-p "Really quit Emacs? ")
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") #'confirm-before-quit)

;; -------------------------
;; Rust + rust-analyzer + tree-sitter
;; -------------------------
(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . lsp)
  (rust-ts-mode . lsp-inlay-hints-mode)
  (rust-ts-mode . lsp-format-buffer-on-save)
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "check")
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-check-on-save t)
  (setq lsp-rust-analyzer-cargo-cfgs [])
  ;; modern inlay hints
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

(defun lsp-format-buffer-on-save ()
  "Add auto-formatting on save for buffers using lsp-mode."
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

;; --- Extra Rust highlighting: semantic tokens + Doom theme enhancements ---

(with-eval-after-load 'lsp-mode
  ;; Ensure semantic tokens specifically for rust-ts-mode
  (add-hook 'rust-ts-mode-hook #'lsp-semantic-tokens-mode))

;; Doom treesitter visual enhancements
(setq doom-themes-treesitter-colored-indent-levels t)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)


;; -------------------------
;; Terraform (.tf, .tfvars) + HCL (.hcl)
;; Syntax highlighting + LSP (definitions/references/hover) + format on save
;; -------------------------

;; 1) Major modes (syntax highlighting)
(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\'"     . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode)))

(use-package hcl-mode
  :ensure t
  :mode (("\\.hcl\\'" . hcl-mode)))

;; 2) (Optional) Prefer tree-sitter modes if you have them available
;;    This won't error if they don't exist.
(with-eval-after-load 'treesit
  (when (fboundp 'terraform-ts-mode)
    (add-to-list 'major-mode-remap-alist '(terraform-mode . terraform-ts-mode)))
  (when (fboundp 'hcl-ts-mode)
    (add-to-list 'major-mode-remap-alist '(hcl-mode . hcl-ts-mode))))

;; 3) LSP wiring
(with-eval-after-load 'lsp-mode
  ;; Make sure LSP knows the language IDs
  (add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))
  (add-to-list 'lsp-language-id-configuration '(hcl-mode       . "hcl"))
  (when (boundp 'lsp-language-id-configuration)
    (when (fboundp 'terraform-ts-mode)
      (add-to-list 'lsp-language-id-configuration '(terraform-ts-mode . "terraform")))
    (when (fboundp 'hcl-ts-mode)
      (add-to-list 'lsp-language-id-configuration '(hcl-ts-mode . "hcl"))))

  ;; Start LSP automatically
  (dolist (hook '(terraform-mode-hook hcl-mode-hook))
    (add-hook hook #'lsp-deferred))
  (when (fboundp 'terraform-ts-mode)
    (add-hook 'terraform-ts-mode-hook #'lsp-deferred))
  (when (fboundp 'hcl-ts-mode)
    (add-hook 'hcl-ts-mode-hook #'lsp-deferred))

  ;; Format on save via LSP
  (defun my/terraform-hcl-lsp-format-on-save ()
    "Format current buffer via LSP (Terraform/HCL)."
    (when (and (bound-and-true-p lsp-mode)
               (derived-mode-p 'terraform-mode 'hcl-mode
                               'terraform-ts-mode 'hcl-ts-mode))
      (lsp-format-buffer)))

  (dolist (hook '(terraform-mode-hook hcl-mode-hook))
    (add-hook hook (lambda ()
                     (add-hook 'before-save-hook
                               #'my/terraform-hcl-lsp-format-on-save
                               nil t))))
  (when (fboundp 'terraform-ts-mode)
    (add-hook 'terraform-ts-mode-hook (lambda ()
                                        (add-hook 'before-save-hook
                                                  #'my/terraform-hcl-lsp-format-on-save
                                                  nil t))))
  (when (fboundp 'hcl-ts-mode)
    (add-hook 'hcl-ts-mode-hook (lambda ()
                                  (add-hook 'before-save-hook
                                            #'my/terraform-hcl-lsp-format-on-save
                                            nil t)))))


;; -------------------------
;; Fix PATH for macOS GUI Emacs
;; -------------------------

;; (add-to-list 'exec-path (expand-file-name "~/go/bin"))

;; (setenv "PATH"
;;         (concat (expand-file-name "~/go/bin")
;;                 ":"
;;                 (getenv "PATH")))

;; -------------------------
;; Go + gopls + tree-sitter
;; -------------------------

(use-package go-ts-mode
  :mode ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . lsp-deferred)
  (go-ts-mode . my/go-lsp-setup)
  :config
  ;; gopls settings
  (setq lsp-go-use-gofumpt t)
  (setq lsp-go-analyses '((unusedparams . t)
                          (shadow . t)))
  (setq lsp-go-staticcheck t)

  ;; inlay hints
  (setq lsp-go-inlay-hints-parameter-names t)
  (setq lsp-go-inlay-hints-variable-types t)
  (setq lsp-go-inlay-hints-constant-values t)
  (setq lsp-go-inlay-hints-function-type-parameters t))

;; -----------------------------------------
;; language-id for go-ts-mode
;; -----------------------------------------
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(go-ts-mode . "go")))

;; -----------------------------------------
;; format + imports on save
;; -----------------------------------------

(defun my/go-lsp-format-on-save ()
  "Format Go buffer using LSP."
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (lsp-format-buffer)))

(defun my/go-lsp-organize-imports ()
  "Organize Go imports via LSP."
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (lsp-organize-imports)))

(defun my/go-lsp-setup ()
  "Setup Go LSP save hooks."
  (add-hook 'before-save-hook #'my/go-lsp-organize-imports nil t)
  (add-hook 'before-save-hook #'my/go-lsp-format-on-save nil t))


;; -------------------------
;; Elixir + ElixirLS + tree-sitter
;; -------------------------

(use-package elixir-ts-mode
:mode ("\.ex\'" . elixir-ts-mode)
("\.exs\'" . elixir-ts-mode)
:hook
(elixir-ts-mode . lsp-deferred)
(elixir-ts-mode . my/elixir-lsp-setup)
:config
;; LSP (ElixirLS) settings
(setq lsp-elixir-fetch-deps t)
(setq lsp-elixir-dialyzer-enabled t))

;; -----------------------------------------
;; language-id for elixir-ts-mode
;; -----------------------------------------
(with-eval-after-load 'lsp-mode
(add-to-list 'lsp-language-id-configuration
'(elixir-ts-mode . "elixir")))

;; -----------------------------------------
;; format + code actions on save
;; -----------------------------------------

(defun my/elixir-lsp-format-on-save ()
"Format Elixir buffer using LSP."
(when (derived-mode-p 'elixir-mode 'elixir-ts-mode)
(lsp-format-buffer)))

(defun my/elixir-lsp-organize-imports ()
"Organize Elixir imports via LSP."
(when (and (derived-mode-p 'elixir-mode 'elixir-ts-mode)
(bound-and-true-p lsp-mode))
(lsp-organize-imports)))

(defun my/elixir-lsp-setup ()
"Setup Elixir LSP save hooks."
(add-hook 'before-save-hook #'my/elixir-lsp-format-on-save nil t)
(add-hook 'before-save-hook #'my/elixir-lsp-organize-imports nil t))

;; -----------------------------------------
;; Ignore heavy Elixir build dirs (performance)
;; -----------------------------------------
(with-eval-after-load 'lsp-mode
(add-to-list 'lsp-file-watch-ignored-directories "[/\\]_build$")
(add-to-list 'lsp-file-watch-ignored-directories "[/\\]deps$"))

;; -------------------------
;; Fix PATH for macOS (brew)
;; -------------------------
(add-to-list 'exec-path "/Users/kanishk/elixir-ls/release")


;; -------------------------
;; Node.js / TypeScript + vtsls + tree-sitter
;; -------------------------

;; Use vtsls (VSCode-level TS/JS LSP)
(add-to-list 'exec-path "/Users/kanishk/.nvm/versions/node/v24.14.0/bin")
(setq lsp-clients-typescript-tls-path
      (or (executable-find "vtsls") "vtsls"))

;; -----------------------------------------
;; language-id for tree-sitter modes
;; -----------------------------------------
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(typescript-ts-mode . "typescript"))
  (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode        . "typescriptreact"))
  (add-to-list 'lsp-language-id-configuration '(js-ts-mode         . "javascript")))

;; -----------------------------------------
;; format + organize imports on save
;; -----------------------------------------

(defun my/ts-lsp-format-on-save ()
  "Format TS/JS buffer using LSP."
  (when (derived-mode-p 'typescript-mode 'typescript-ts-mode
                        'js-mode 'js-ts-mode 'tsx-ts-mode)
    (lsp-format-buffer)))

(defun my/ts-lsp-organize-imports ()
  "Organize imports via LSP."
  (when (and (derived-mode-p 'typescript-mode 'typescript-ts-mode
                             'js-mode 'js-ts-mode 'tsx-ts-mode)
             (bound-and-true-p lsp-mode))
    (lsp-organize-imports)))

(defun my/ts-lsp-setup ()
  "Setup Node/TS LSP save hooks."
  (add-hook 'before-save-hook #'my/ts-lsp-organize-imports nil t)
  (add-hook 'before-save-hook #'my/ts-lsp-format-on-save nil t))

;; -----------------------------------------
;; tree-sitter modes
;; -----------------------------------------

(use-package typescript-ts-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :hook
  (typescript-ts-mode . lsp-deferred)
  (typescript-ts-mode . my/ts-lsp-setup))

(use-package tsx-ts-mode
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook
  (tsx-ts-mode . lsp-deferred)
  (tsx-ts-mode . my/ts-lsp-setup))

;; JS (already remapped, just ensure hooks)
(add-hook 'js-ts-mode-hook #'lsp-deferred)
(add-hook 'js-ts-mode-hook #'my/ts-lsp-setup)

;; -----------------------------------------
;; ESLint (built into lsp-mode)
;; -----------------------------------------

(with-eval-after-load 'lsp-mode
  ;; Enable ESLint integration
  (setq lsp-eslint-enable t)
  (setq lsp-eslint-format t)
  (setq lsp-eslint-run "onType"))

(defun my/eslint-apply-fixes ()
  "Apply ESLint fixes on save."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/codeAction"))
    (lsp-execute-code-action-by-kind "source.fixAll.eslint")))

(dolist (hook '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook))
  (add-hook hook
            (lambda ()
              (add-hook 'before-save-hook #'my/eslint-apply-fixes nil t))))
;; -----------------------------------------
;; Ignore heavy Node dirs (performance)
;; -----------------------------------------

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]dist$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.next$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]out$"))

;; -----------------------------------------
;; vtsls tuning
;; -----------------------------------------

;; These are the `lsp-javascript` knobs used by the TS/JS client in lsp-mode.
(setq lsp-javascript-suggest-auto-imports t)
(setq lsp-typescript-suggest-auto-imports t)
(setq lsp-clients-typescript-max-ts-server-memory 4096)
(setq lsp-javascript-completions-complete-function-calls t)

;; -----------------------------------------
;; Performance tweaks (important for Node)
;; -----------------------------------------

(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq gc-cons-threshold 100000000)

;; ====================
;; Doom Theme (daemon-safe)
;; ====================
(use-package doom-themes
  :ensure t
  :init
  ;; Make sure themes are loaded after initialization to avoid partial face setup
  (add-hook 'after-init-hook
            (lambda ()
              (load-theme 'doom-dark+ t)))
  :config
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))


;;; kanishk-conf.el ends here
