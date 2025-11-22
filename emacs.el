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
        (go-mode     . go-ts-mode)))

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
  (setq lsp-semantic-tokens-enable t)   ;; enable semantic tokens
  :hook
  ((python-mode   . lsp)
   (python-ts-mode . lsp)
   (go-mode       . lsp)
   (go-ts-mode    . lsp)
   (js-mode       . lsp)
   (js-ts-mode    . lsp))
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
;; Python Enhancements
;; ====================
(with-eval-after-load 'python
  ;; Jump between blocks at same indentation
  (define-key python-mode-map (kbd "M-n") #'python-nav-forward-block)
  (define-key python-mode-map (kbd "M-p") #'python-nav-backward-block)

  ;; Jump up/down indentation levels
  (define-key python-mode-map (kbd "M-u") #'python-nav-backward-up-list) ;; up to parent
  (define-key python-mode-map (kbd "M-d") #'python-nav-forward-statement)) ;; down into child

;; ====================
;; Git Diff Fringe
;; ====================
(global-diff-hl-mode +1)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; Optional keybindings for navigating hunks
(global-set-key (kbd "C-x v n") 'diff-hl-next-hunk)
(global-set-key (kbd "C-x v p") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-x v r") 'diff-hl-revert-hunk)

(setq diff-hl-fringe-face-function
      'diff-hl-fringe-face-from-type)

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
(with-eval-after-load 'lsp-mode
  (setq lsp-completion-provider :capf))
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
;; Nuclear option: Disable Company completely
;; ============================
(setq prelude-company nil)

(when (featurep 'company)
  (global-company-mode -1)
  (unload-feature 'company t))

(with-eval-after-load 'company
  (global-company-mode -1))

(defun my/kill-company-in-buffer ()
  "Ensure company-mode is off in current buffer."
  (when (bound-and-true-p company-mode)
    (company-mode -1)))
(add-hook 'after-change-major-mode-hook #'my/kill-company-in-buffer)

(dolist (hook '(prog-mode-hook
                python-mode-hook
                emacs-lisp-mode-hook
                go-mode-hook))
  (add-hook hook (lambda () (company-mode -1)) 90))

(defun my/prevent-company-mode (orig-fun &optional arg &rest args)
  "Prevent company-mode from being enabled."
  (when (or (not arg) (<= arg 0))
    (apply orig-fun arg args)))
(advice-add 'company-mode :around #'my/prevent-company-mode)

;; ============================
;; Disable anaconda mode entirely
;; ============================
(use-package anaconda-mode
  :disabled t)
(use-package company-anaconda
  :disabled t)

(with-eval-after-load 'python
  (remove-hook 'python-mode-hook 'anaconda-mode)
  (remove-hook 'python-mode-hook 'anaconda-eldoc-mode))

(with-eval-after-load 'anaconda-mode
  (setq anaconda-mode nil)
  (setq anaconda-eldoc-mode nil)
  (when (boundp 'anaconda-mode-map)
    (setcdr anaconda-mode-map nil)))

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

;;; kanishk-conf.el ends here
