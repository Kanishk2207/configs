;;; kanishk-conf.el --- Personal configuration

;; ====================
;; Package / MELPA
;; ====================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))


;; ====================
;; Emacs 30: use tree-sitter modes
;; ====================
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode     . js-ts-mode)
        (css-mode    . css-ts-mode)
        (c-mode      . c-ts-mode)
        (c++-mode    . c++-ts-mode)
        (go-mode     . go-ts-mode)
        (rust-mode   . rust-ts-mode)))

(setq treesit-font-lock-level 4)


;; ====================
;; Eglot (LSP Client)
;; ====================

(use-package eglot
  :ensure t
  :hook ((python-mode python-ts-mode
          go-mode go-ts-mode
          js-mode js-ts-mode
          rust-mode rust-ts-mode)
         . eglot-ensure)
  :config
  ;; Slightly quieter Eglot, shuts down cleanly
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)

  ;; Inlay hints for languages that support them
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

  ;; rust-analyzer manually registered for rust-ts-mode
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"))))

(with-eval-after-load 'eglot
  (set-face-attribute 'eglot-inlay-hint-face nil
                      :foreground "#88c0d0"
                      :background "#3b4252"
                      :box '(:line-width -1 :color "#4c566a")
                      :height 0.9)
  (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil
                      :underline '(:style wave :color "cyan")
                      :foreground "cyan"))


;; ====================
;; Keybindings (Xref-based navigation)
;; ====================


(use-package embark
  :ensure t)

(use-package consult
  :ensure t)

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package consult-eglot-embark
  :ensure t
  :after (consult embark eglot))

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Disable smartparens stealing M-?
(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map (kbd "M-?") nil))

(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-?") #'xref-find-references)
(global-set-key (kbd "M-,") #'xref-go-back)

;; Code actions (Ruff fixes, Pyright suggestions, Rust actions)
(global-set-key (kbd "C-c C-a") #'eglot-code-actions)

;; ====================
;; Python Setup (Pyright + Ruff + formatting)
;; ====================

;; Auto-detect virtualenv
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-tracking-mode 1))

;; ---- Ruff Autofix on save ----
(defun my/ruff-autofix ()
  "Apply Ruff LSP auto-fixes using Eglot."
  (when (and (eglot-managed-p)
             (eglot-code-action-organize-imports-supported))
    (eglot-code-actions
     (point-min)
     (point-max)
     "source.fixAll.ruff")))

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/ruff-autofix nil t)))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/ruff-autofix nil t)))


;; ---- Python Formatting via Pyright ----
(defun my/python-format-on-save ()
  "Format Python buffer using Eglot (Pyright)."
  (when (eglot-managed-p)
    (eglot-format)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/python-format-on-save nil t)))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/python-format-on-save nil t)))


;; ---- Ruff Flymake linting ----
(use-package flymake-ruff
  :ensure t
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))


;; ====================
;; Rust Setup
;; ====================

(add-hook 'rust-ts-mode-hook
          (lambda ()
            (flycheck-mode -1)
            (add-hook 'before-save-hook #'eglot-format nil t)))


;; ====================
;; Corfu Setup
;; ====================

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

(add-hook 'eglot-managed-mode-hook #'corfu-mode)


;; Better fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((eglot (styles orderless))
     (lsp-capf (styles orderless)))))

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
;; Python Enhancements TODO: add venv restart and along with lsp restart on project switch
;; ====================
(with-eval-after-load 'python
  ;; Jump between blocks at same indentation
  (define-key python-ts-mode-map (kbd "M-n") #'python-nav-forward-block)
  (define-key python-ts-mode-map (kbd "M-p") #'python-nav-backward-block)

  ;; Jump up/down indentation levels
  (define-key python-ts-mode-map (kbd "M-u") #'python-nav-backward-up-list) ;; up to parent
  (define-key python-ts-mode-map (kbd "M-d") #'python-nav-forward-statement)) ;; down into child


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

;; ====================
;; Move line up/down with Super + ↑ / ↓
;; ====================
(use-package move-text
  :ensure t
  :config
  (global-set-key (kbd "s-<up>") 'move-text-up)
  (global-set-key (kbd "s-<down>") 'move-text-down))

;; ============================
;; Confirm before quitting
;; ============================
(defun confirm-before-quit ()
  "Ask for confirmation before quitting Emacs."
  (interactive)
  (when (yes-or-no-p "Really quit Emacs? ")
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") #'confirm-before-quit)

;; ====================
;; Doom Theme (daemon-safe)
;; ====================
(use-package doom-themes
 :ensure t
 :init
 ;; Make sure themes are loaded after initialization to avoid partial face setup
 (add-hook 'after-init-hook
           (lambda ()
             (load-theme 'doom-one t)))
 :config
 (doom-themes-org-config)
 (doom-themes-visual-bell-config))

;; Doom treesitter visual enhancements
(setq doom-themes-treesitter-colored-indent-levels t)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

;;; kanishk-conf.el ends here
