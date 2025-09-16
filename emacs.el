;;; kanishk-conf.el --- Personal configuration

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
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))


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
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))


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
                      :foreground "dim gray"))

;;; kanishk-conf.el ends here
