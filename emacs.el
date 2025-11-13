;;; kanishk-conf.el --- Personal configuration

;; ====================
;; enable MELPA
;; ====================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ====================
;; GUI Setup (daemon-safe)
;; ====================
(defun my/gui-setup (frame)
  (with-selected-frame frame
    ;; Disable previous theme and load Wombat
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'wombat t)

    ;; Cursor
    (setq-default cursor-type 'bar)
    (set-cursor-color "white")
    (blink-cursor-mode 1)

    ;; Git diff fringes
    (global-diff-hl-mode +1)
    (diff-hl-dired-mode)))


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
  ((python-mode . lsp)
   (go-mode     . lsp)
   ;; add more modes as you like
   )
  :commands lsp)

;; LSP UI
(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  :hook
  (lsp-mode . lsp-ui-mode)) ;; lsp-ui-peek will still work

;; ====================
;; Whitespace Mode
;; ====================
(setq whitespace-style
      '(face trailing space-before-tab empty space-after-tab))

(global-whitespace-mode 1)


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
;; Hooks already applied in gui-setup, but keep non-GUI parts here
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; Optional keybindings for navigating hunks
(global-set-key (kbd "C-x v n") 'diff-hl-next-hunk)
(global-set-key (kbd "C-x v p") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-x v r") 'diff-hl-revert-hunk)

(setq diff-hl-fringe-face-function
      'diff-hl-fringe-face-from-type)

;;; kanishk-conf.el ends here
;;; kanishk-conf.el --- Personal configuration

;; ====================
;; GUI Setup (daemon-safe)
;; ====================
(defun my/gui-setup (frame)
  (with-selected-frame frame
    ;; Disable Zenburn (Prelude’s default) before loading Wombat
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'wombat t)

    ;; Cursor
    (setq-default cursor-type 'bar)
    (set-cursor-color "white")
    (blink-cursor-mode 1)

    ;; Git diff fringes
    (global-diff-hl-mode +1)
    (diff-hl-dired-mode)
    ))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/gui-setup)
  (my/gui-setup (selected-frame)))


;; ==========================
;; Permanent white cursor
;; ==========================
(defvar my/cursor-color "white"
  "The only cursor color allowed. Change this var to alter the color manually.")

(defun my/lock-cursor-color (&rest _)
  "Force the cursor color to stay `my/cursor-color`, ignoring theme changes."
  (let ((current (frame-parameter nil 'cursor-color)))
    (unless (string= current my/cursor-color)
      (set-cursor-color my/cursor-color)
      ;; Also override theme face
      (custom-set-faces
       `(cursor ((t (:background ,my/cursor-color))))))))

;; Apply immediately
(my/lock-cursor-color)

;; Re-apply whenever a theme loads (themes love to reset cursor)
(advice-add 'load-theme :after #'my/lock-cursor-color)

;; Re-apply when new frames are created (daemon-safe)
(add-hook 'after-make-frame-functions
          (lambda (f)
            (with-selected-frame f
              (my/lock-cursor-color))))

;; Re-apply if something else changes it (just in case)
(run-with-timer 0 10 #'my/lock-cursor-color) ;; every 10 seconds, silently reassert


;; ====================
;; LSP Mode
;; ====================
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-semantic-tokens-enable t)   ;; enable semantic tokens
  :hook
  ((python-mode . lsp)
   (go-mode     . lsp)
   ;; add more modes as you like
   )
  :commands lsp)

;; LSP UI
(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  :hook
  (lsp-mode . lsp-ui-mode)) ;; lsp-ui-peek will still work


;; ====================
;; Whitespace Mode
;; ====================
(setq whitespace-style
      '(face trailing space-before-tab empty space-after-tab))

(global-whitespace-mode 1)


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
;; Force Ediff to use horizontal (side-by-side) split
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;; ====================
;; Swiper
;; ====================
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)          ;; search in current buffer
         ("C-r" . swiper)          ;; backwards search
         ("C-c C-s" . swiper-all))) ;; search across all open buffers


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
  (setq aw-dispatch-always t)  ;; always show letters
  (setq aw-scope 'frame))      ;; only current frame

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
;; set selection yellow
;; ====================
(defun my/set-region-color (&rest _)
  "Force region (selection) to yellow regardless of theme."
  (set-face-attribute 'region nil
                      :background "yellow"
                      :foreground "black"))

;; Apply once immediately
(my/set-region-color)

;; Reapply after theme loads
(advice-add 'load-theme :after #'my/set-region-color)

;; Reapply when a new frame is created (daemon-safe)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my/set-region-color))))


;; ====================
;; change ace window shortcut to C-c o
;; ====================
(define-key prelude-mode-map (kbd "C-c o") nil)

;; Rebind ace-window
(global-set-key (kbd "C-c o") 'ace-window)
(global-set-key (kbd "C-c C-o") 'ace-window)

;; ====================
;; set trailing whitespace to grey instead of yellow
;; ====================
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

;; set key-bindings for cursor below and cursor above
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

;; flycher limit change
(with-eval-after-load 'flycheck
  (setq flycheck-checker-error-threshold 2000))

; for smooth scrolling
(use-package pixel-scroll
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-interpolation-factor 0.00005)
  :init
  (pixel-scroll-precision-mode 1))

;; Set font size globally
(set-face-attribute 'default nil :height 105)

;; ====================
;; Magit window split direction
;; ====================
(with-eval-after-load 'magit
  ;; Restore the classic Magit window behavior
  (setq magit-display-buffer-function #'magit-display-buffer-traditional)

  ;; Force vertical splits (side-by-side)
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

;; ---------- Corfu Setup ----------
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode) ;; optional docs popup
  :custom
  ;; Popup behavior
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preselect 'first)
  (corfu-scroll-margin 4)

  ;; Quit and preview controls
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)

  ;; Aesthetic tweaks
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
        ("RET" . corfu-insert)   ;; ✅ Make Return insert selected completion
        ("TAB" . corfu-next)
        ("S-TAB" . corfu-previous)))


;; ---------- LSP Integration ----------
(with-eval-after-load 'lsp-mode
  (setq lsp-completion-provider :capf))

(add-hook 'lsp-mode-hook #'corfu-mode)


;; ====================
;; Move line up/down with Super + ↑ / ↓
;; ====================
(use-package move-text
  :ensure t
  :config
  ;; Don't override smartparens' M-<up>/<down>
  (global-set-key (kbd "s-<up>") 'move-text-up)
  (global-set-key (kbd "s-<down>") 'move-text-down))


;; ============================
;; Nuclear option: Disable Company completely
;; ============================

;; 1. Prevent company from ever loading
(setq prelude-company nil)  ;; Tell Prelude not to use Company

;; 2. Unload company if already loaded
(when (featurep 'company)
  (global-company-mode -1)
  (unload-feature 'company t))

;; 3. Prevent loading via autoload
(with-eval-after-load 'company
  (global-company-mode -1))

;; 4. Remove from all major mode hooks
(defun my/kill-company-in-buffer ()
  "Ensure company-mode is off in current buffer."
  (when (bound-and-true-p company-mode)
    (company-mode -1)))

(add-hook 'after-change-major-mode-hook #'my/kill-company-in-buffer)

;; 5. Specifically for programming modes (where Prelude likes to enable it)
(dolist (hook '(prog-mode-hook
                python-mode-hook
                emacs-lisp-mode-hook
                go-mode-hook))
  (add-hook hook (lambda () (company-mode -1)) 90))  ;; Run late to override Prelude

;; 6. If somehow company gets enabled, immediately disable it
(defun my/prevent-company-mode (orig-fun &optional arg &rest args)
  "Prevent company-mode from being enabled."
  (when (or (not arg) (<= arg 0))
    (apply orig-fun arg args)))

(advice-add 'company-mode :around #'my/prevent-company-mode)

;; ============================
;; Disable anaconda mode entirly
;; ============================

;; Stop it from ever loading
(use-package anaconda-mode
  :disabled t)

(use-package company-anaconda
  :disabled t)

;; Remove any hooks Prelude or other packages might have added
(with-eval-after-load 'python
  (remove-hook 'python-mode-hook 'anaconda-mode)
  (remove-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; If anaconda-mode somehow loads anyway, kill its keymap
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


(defun confirm-before-quit ()
  "Ask for confirmation before quitting Emacs."
  (interactive)
  (when (yes-or-no-p "Really quit Emacs? ")
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") #'confirm-before-quit)

;;; kanishk-conf.el ends here
