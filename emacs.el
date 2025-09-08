 ;; Theme
(load-theme 'wombat t)

;; LSP Mode
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

;; Keep whitespace-mode, but no long-line highlighting
(setq whitespace-style
      '(face trailing space-before-tab empty space-after-tab))

;; Reload whitespace-mode with new settings
(global-whitespace-mode 1)

;; Make cursor a thin blinking bar
(setq-default cursor-type 'bar)
(set-cursor-color "white")
(blink-cursor-mode 1)

;; projectile dired mode
(with-eval-after-load 'projectile
  (setq projectile-switch-project-action #'projectile-dired))


;; Bind M-? globally to lsp-ui-peek-find-references
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'lsp-ui

    ;; 1. Unbind M-? from minor modes that override it
    (with-eval-after-load 'smartparens
      (define-key smartparens-mode-map (kbd "M-?") nil))
    (with-eval-after-load 'anaconda-mode
      (define-key anaconda-mode-map (kbd "M-?") nil))

    ;; 2. Set up keybindings for LSP navigation
    ;; Go to definition
    (global-set-key (kbd "M-.") #'lsp-find-definition)
    ;; Peek references
    (global-set-key (kbd "M-?") #'lsp-ui-peek-find-references)
    ;; Jump back
    (global-set-key (kbd "M-,") #'xref-pop-marker-stack)

    ;; 3. Optional: make M-? local in programming buffers
    ;; This ensures Smartparens docs still work elsewhere
    (add-hook 'prog-mode-hook
              (lambda ()
                (local-set-key (kbd "M-?") #'lsp-ui-peek-find-references)))))

;; Add to your prelude-personal.el
(with-eval-after-load 'python
  ;; Jump between blocks at same indentation
  (define-key python-mode-map (kbd "M-n") #'python-nav-forward-block)
  (define-key python-mode-map (kbd "M-p") #'python-nav-backward-block)

  ;; Jump up/down indentation levels
  (define-key python-mode-map (kbd "M-u") #'python-nav-backward-up-list) ;; up to parent
  (define-key python-mode-map (kbd "M-d") #'python-nav-forward-statement)) ;; down into child


;; git fringes
;; Enable globally
(global-diff-hl-mode +1)

;; Refresh diff-hl after Magit operations
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Optional: enable diff-hl in dired buffers
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; Optional keybindings for navigating hunks
(global-set-key (kbd "C-x v n") 'diff-hl-next-hunk)
(global-set-key (kbd "C-x v p") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-x v r") 'diff-hl-revert-hunk)

;; Optional: highlight current line's hunk in fringe
(setq diff-hl-fringe-face-function
      'diff-hl-fringe-face-from-type)
