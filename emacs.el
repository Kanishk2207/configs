;;; kanishk-conf.el --- Personal configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Personal settings layered on top of Prelude.
;; Loaded from personal/ after all Prelude modules.
;;
;; Layout:
;;   1.  Packages & performance
;;   2.  macOS environment (PATH, gls)
;;   3.  Tree-sitter
;;   4.  Frames & appearance (daemon-safe: runs per-frame)
;;   5.  Scrolling
;;   6.  Editing, utilities & global keybindings
;;   7.  Completion (Corfu + Orderless + Swiper)
;;   8.  Windows, Dired, Ediff, Magit, Projectile, Flycheck
;;   9.  Whitespace & TODO highlighting
;;   10. LSP core (lsp-mode, lsp-ui, navigation keys)
;;   11. Languages: Python, Go, Rust, Clojure, Elixir, TS/JS, Terraform/HCL
;;   12. Theme (always last)
;;
;; Daemon notes:
;;   Emacs runs as a daemon (emacs --daemon) and serves BOTH GUI and
;;   terminal (tty) clients.  Anything frame-dependent (colors, cursor,
;;   fringes, scroll bars) is applied through `my/frame-setup', which is
;;   hooked on `after-make-frame-functions' and guards GUI-only bits
;;   with `display-graphic-p'.  Global settings (faces, modes, keymaps)
;;   are safe to set once at load time.

;;; Code:

;; ============================================================
;; 1. Packages & performance
;; ============================================================

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("melpa"  . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(eval-when-compile
  (require 'use-package))

;; Large subprocess reads + relaxed GC: important for LSP servers.
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq gc-cons-threshold 100000000)

;; ============================================================
;; 2. macOS environment
;; ============================================================

;; GNU ls from coreutils, for dired switches.
(setq insert-directory-program "gls")

;; Binaries the daemon must see regardless of which shell launched it.
(dolist (dir '("/Users/kanishk/elixir-ls/release"
               "/Users/kanishk/.nvm/versions/node/v24.14.0/bin"))
  (add-to-list 'exec-path dir))

;; ============================================================
;; 3. Tree-sitter (Emacs 30 built-in modes)
;; ============================================================

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

;; ============================================================
;; 4. Frames & appearance (daemon-safe)
;; ============================================================

(defvar my/cursor-color "white"
  "The only cursor color allowed.  Change this to alter the cursor color.")

;; Global defaults: safe at load time, apply to every future frame.
(setq-default cursor-type 'bar)          ;; tty renders this as a block; fine
(set-face-attribute 'default nil :height 140)

;; Scroll bars: the macOS NS/Cocoa toolkit scroller has a fixed native
;; width (~15 px) and refuses to draw a knob for narrow `scroll-bar-width'
;; values -- they just reserve blank space.  So a *thin* native scroll
;; bar is impossible on this build.  We turn the native scroll bars off
;; here and draw a thin one with `yascroll' in the right fringe (below).
(modify-all-frames-parameters
 '((vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)))

(defun my/lock-cursor-color (&rest _)
  "Force the cursor color to stay `my/cursor-color', ignoring theme changes."
  (set-cursor-color my/cursor-color)
  (set-face-attribute 'cursor nil :background my/cursor-color))

(defun my/set-region-color (&rest _)
  "Force region (selection) to yellow regardless of theme."
  (set-face-attribute 'region nil
                      :background "yellow"
                      :foreground "black"))

(defun my/frame-setup (&optional frame)
  "Apply per-frame settings to FRAME (or the selected frame).
Safe for both GUI and terminal frames: GUI-only tweaks are guarded."
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (my/lock-cursor-color)
      (my/set-region-color)
      (when (display-graphic-p frame)
        ;; No native scroll bars; yascroll draws a thin one in the fringe.
        (set-frame-parameter frame 'vertical-scroll-bars nil)
        ;; Left fringe 4 px (diagnostics), right fringe 8 px (yascroll thumb).
        (fringe-mode '(4 . 3))))))

;; Run for every client frame the daemon creates...
(add-hook 'after-make-frame-functions #'my/frame-setup)
;; ...and immediately when Emacs is started non-daemon.
(unless (daemonp)
  (my/frame-setup))

;; Re-apply the color locks whenever a theme loads.
(advice-add 'load-theme :after #'my/lock-cursor-color)
(advice-add 'load-theme :after #'my/set-region-color)

;; Thin scroll bar: `yascroll' draws a one-fringe-wide indicator in the
;; right fringe of every window.  This is the reliable way to get a
;; genuinely thin scroll bar on the NS build (see the note above).  Widen
;; the right fringe if the thumb is too subtle for your taste.
(use-package yascroll
  :ensure t
  :init
  (scroll-bar-mode -1)                 ;; kill the native NS scroller
  (global-yascroll-bar-mode 1)
  :custom
  (yascroll:delay-to-hide nil)         ;; nil = always visible (never auto-hide)
  (yascroll:scroll-bar '(right-fringe)))

;; ============================================================
;; 5. Scrolling
;; ============================================================

;; Pixel-perfect scrolling in GUI frames; plain paging on a tty.
;; The wrappers matter under the daemon: a bare [remap] to the pixel
;; commands would break C-v / M-v in terminal clients.
(use-package pixel-scroll
  :init
  (pixel-scroll-precision-mode 1)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-interpolation-factor 0.00005))

(defun my/scroll-up (&optional arg)
  "Pixel-scroll down a page in GUI frames, plain `scroll-up-command' on tty."
  (interactive "^P")
  (if (and (display-graphic-p) pixel-scroll-precision-mode)
      (pixel-scroll-interpolate-down)
    (scroll-up-command arg)))

(defun my/scroll-down (&optional arg)
  "Pixel-scroll up a page in GUI frames, plain `scroll-down-command' on tty."
  (interactive "^P")
  (if (and (display-graphic-p) pixel-scroll-precision-mode)
      (pixel-scroll-interpolate-up)
    (scroll-down-command arg)))

(global-set-key [remap scroll-up-command]   #'my/scroll-up)
(global-set-key [remap scroll-down-command] #'my/scroll-down)

;; ============================================================
;; 6. Editing, utilities & global keybindings
;; ============================================================

;; Redo (undo-tree comes with Prelude).
(global-set-key (kbd "M-/") #'undo-tree-redo)

;; Move line/region with Super + arrows (GUI; most terminals don't
;; deliver the Super modifier, which is fine per-client).
(use-package move-text
  :ensure t
  :bind (("s-<up>"   . move-text-up)
         ("s-<down>" . move-text-down)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

(defun kill-ring-delete-entry (string)
  "Delete STRING from the kill-ring."
  (interactive
   (list (completing-read "Delete from kill-ring: " kill-ring)))
  (setq kill-ring (delete string kill-ring))
  (message "Deleted: %s" string))

(defun confirm-before-quit ()
  "Ask for confirmation before quitting Emacs."
  (interactive)
  (when (yes-or-no-p "Really quit Emacs? ")
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") #'confirm-before-quit)

;; ============================================================
;; 7. Completion (Corfu + Orderless + Swiper)
;; ============================================================

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
        ("C-n"    . corfu-next)
        ("C-p"    . corfu-previous)
        ("<down>" . corfu-next)
        ("<up>"   . corfu-previous)
        ("M-RET"  . corfu-insert)
        ("RET"    . corfu-insert)
        ("TAB"    . corfu-next)
        ("S-TAB"  . corfu-previous)))

;; Fuzzy matching for Corfu + LSP.
(use-package orderless
  :ensure t
  :custom
  ;; Keep `basic' as a fallback and `partial-completion' for file names
  ;; (so /u/l/b -> /usr/local/bin still works); add orderless for lsp-capf.
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (lsp-capf (styles orderless)))))

(use-package swiper
  :ensure t
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper-backward)
         ("C-c C-s" . swiper-all)))

;; ============================================================
;; 8. Windows, Dired, Ediff, Magit, Projectile, Flycheck, Dotenv
;; ============================================================

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("s-w"   . ace-window))
  :config
  (setq aw-dispatch-always t)
  (setq aw-scope 'frame))

;; Free C-c o from Prelude and give it to ace-window.
(with-eval-after-load 'prelude-mode
  (define-key prelude-mode-map (kbd "C-c o") nil))
(global-set-key (kbd "C-c o")   #'ace-window)
(global-set-key (kbd "C-c C-o") #'ace-window)

(use-package dired-quick-sort
  :ensure t
  :config
  ;; Group directories first, then files (GNU ls --group-directories-first).
  ;; dired-quick-sort owns the ls switches, so this toggle -- not
  ;; `dired-listing-switches' -- is what controls grouping.  You can still
  ;; flip it live in a dired buffer with `S' then `g'.
  (setq dired-quick-sort-group-directories-last ?y)
  (dired-quick-sort-setup))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(with-eval-after-load 'magit
  (setq magit-display-buffer-function #'magit-display-buffer-traditional)
  ;; Open magit's windows to the RIGHT (side by side), not below.  Scoped
  ;; to magit's own display call, so the rest of Emacs keeps its default
  ;; splitting -- this is the magit-only version of the old global
  ;; split-width-threshold/split-height-threshold trick.
  (defun my/magit-split-right (fn &rest args)
    "Force magit to split windows horizontally (to the right)."
    (let ((split-height-threshold nil)   ;; never stack below
          (split-width-threshold 0))     ;; always allow a split to the right
      (apply fn args)))
  (advice-add 'magit-display-buffer :around #'my/magit-split-right))

(with-eval-after-load 'projectile
  (setq projectile-switch-project-action #'projectile-dired))

(with-eval-after-load 'flycheck
  (setq flycheck-checker-error-threshold 2000))

(use-package dotenv-mode
  :ensure t)

;; ============================================================
;; 9. Whitespace & TODO highlighting
;; ============================================================

(setq whitespace-style
      '(face trailing space-before-tab empty space-after-tab))
(global-whitespace-mode 1)

(with-eval-after-load 'whitespace
  ;; Trailing spaces: red
  (set-face-attribute 'whitespace-trailing nil
                      :background "red"
                      :foreground 'unspecified)
  ;; Empty lines at buffer end: grey
  (set-face-attribute 'whitespace-empty nil
                      :background "grey20"
                      :foreground 'unspecified)
  ;; Tabs: subtle grey underline
  (set-face-attribute 'whitespace-tab nil
                      :background 'unspecified
                      :underline t
                      :foreground "dim gray")
  (set-face-attribute 'whitespace-indentation nil
                      :background 'unspecified
                      :foreground 'unspecified))

(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . hl-todo-mode))
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"  warning bold)
          ("FIXME" error bold)
          ("NOTE"  success bold)
          ("INFO"  font-lock-doc-face bold)
          ("DEBUG" font-lock-constant-face bold))))

;; ============================================================
;; 10. LSP core
;; ============================================================

(use-package lsp-mode
  :ensure t
  :init
  ;; Corfu + completion-at-point-functions setup
  ;; (avoids company-mode autoconfig warnings).
  (setq lsp-completion-provider :capf)
  ;; Silence snippet warning when yasnippet isn't installed.
  (setq lsp-enable-snippet nil)
  ;; Semantic highlighting from the language server: colors functions,
  ;; macros, keywords, locals, and definitions that the tree-sitter grammar
  ;; can't distinguish on its own.  Biggest visual win for Clojure; also
  ;; enriches Python/Go/Rust/TS.  Applies to every server that supports it.
  (setq lsp-semantic-tokens-enable t)
  :hook
  ((python-mode    . lsp)
   (python-ts-mode . lsp)
   (go-mode        . lsp)
   (go-ts-mode     . lsp)
   (js-mode        . lsp)
   (js-ts-mode     . lsp)
   (rust-mode      . lsp)
   (rust-ts-mode   . lsp))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
  :hook
  (lsp-mode . lsp-ui-mode))

;; Generic helper: format via LSP before saving (buffer-local hook).
(defun lsp-format-buffer-on-save ()
  "Add auto-formatting on save for buffers using lsp-mode."
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

;; Refresh semantic tokens after theme changes for all LSP buffers.
(with-eval-after-load 'lsp-mode
  (defun my/lsp-refresh-semantic-tokens-after-theme (&rest _)
    "Refresh LSP semantic tokens in all buffers after a theme change."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p lsp-mode)
          (ignore-errors
            (lsp-semantic-tokens-refresh))))))
  (advice-add 'load-theme :after #'my/lsp-refresh-semantic-tokens-after-theme))

;; Language IDs for tree-sitter modes lsp-mode doesn't know yet.
(with-eval-after-load 'lsp-mode
  (dolist (entry '((python-ts-mode     . "python")
                   (go-ts-mode         . "go")
                   (elixir-ts-mode     . "elixir")
                   (typescript-ts-mode . "typescript")
                   (tsx-ts-mode        . "typescriptreact")
                   (js-ts-mode         . "javascript")))
    (add-to-list 'lsp-language-id-configuration entry)))

;; Keep the file watcher out of heavy build/dependency dirs.
(with-eval-after-load 'lsp-mode
  (dolist (dir '("[/\\\\]node_modules$"
                 "[/\\\\]dist$"
                 "[/\\\\]build$"
                 "[/\\\\]\\.next$"
                 "[/\\\\]out$"
                 "[/\\\\]_build$"      ;; Elixir
                 "[/\\\\]deps$"        ;; Elixir
                 "[/\\\\]target$"      ;; Clojure/Rust
                 "[/\\\\]\\.cpcache$"  ;; Clojure
                 "[/\\\\]\\.shadow-cljs$"))
    (add-to-list 'lsp-file-watch-ignored-directories dir)))

;; --- LSP navigation keybindings ---
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'lsp-ui
    ;; Unbind M-? from minor modes that override it.
    (with-eval-after-load 'smartparens
      (define-key smartparens-mode-map (kbd "M-?") nil))
    (with-eval-after-load 'anaconda-mode
      (define-key anaconda-mode-map (kbd "M-?") nil))

    ;; Bind in lsp-mode-map so these are only live where LSP is active;
    ;; non-LSP buffers (elisp, org, dired) keep normal xref/M-. behavior.
    (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
    (define-key lsp-mode-map (kbd "M-?") #'lsp-ui-peek-find-references)
    (define-key lsp-mode-map (kbd "M-,") #'xref-pop-marker-stack)))

;; --- Open definition / references in a side-by-side (vertical) window ---

(defun my/lsp-find-definition-other-window ()
  "Find definition of symbol at point, showing it in a side-by-side window.
Uses lsp-mode's own display-action (`window' = other window) and forces
`display-buffer' to split left/right instead of stacking."
  (interactive)
  (let ((split-width-threshold 0)      ;; always allow a side-by-side split
        (split-height-threshold nil))  ;; never stack top/bottom
    (lsp-find-definition :display-action 'window)))

(defun my/lsp-ui-peek-goto-xref-vsplit ()
  "From an active lsp-ui peek, open the selected reference side by side."
  (interactive)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (lsp-ui-peek--goto-xref-other-window)))

;; C-M-. : definition in another (vertical) window; M-. stays same-window.
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-M-.") #'my/lsp-find-definition-other-window))

;; C-<return> inside the peek : jump to the reference in another (vertical) window.
(with-eval-after-load 'lsp-ui-peek
  (define-key lsp-ui-peek-mode-map (kbd "C-<return>")
              #'my/lsp-ui-peek-goto-xref-vsplit))

;; ============================================================
;; Claude Code (claude-code-ide.el over MCP)
;; ============================================================

;; Pure-Elisp terminal backend: no native compile, daemon-friendly.
(use-package eat
  :ensure t)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide-insert-at-mentioned)   ;; autoload stub for the wrapper
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("M-i"     . my/claude-add-region-or-tab))
  :custom
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-no-flicker t)
  :config
  (claude-code-ide-emacs-tools-setup))

(defun my/claude-add-region-or-tab ()
  "Region active: send it to Claude Code's context.
No region: fall back to `tab-to-tab-stop'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'claude-code-ide-insert-at-mentioned)
    (call-interactively #'tab-to-tab-stop)))

;; --- C-x + that treats the Claude side window as one column ---
;;
;; claude-code-ide shows Claude in a SIDE window.  `balance-windows'
;; balances the window TREE level by level: at the top level the frame
;; has two children (the main-window group and the Claude side window),
;; so Claude gets a full half of the frame and all the normal windows
;; split the other half.  This wrapper instead gives each left/right
;; side window a single equal column share, then balances only the
;; main-window subtree in the remaining space -- calling plain
;; `balance-windows' afterwards would re-inflate Claude to half.

(defun my/main-window-columns (&optional frame)
  "Number of side-by-side columns in FRAME's main (non-side) window area."
  (let* ((main (window-main-window frame))
         (child (window-chwild main)))
    (if (and child (window-combined-p child t))
        (let ((n 0))
          (while child
            (setq n (1+ n)
                  child (window-next-sibling child)))
          n)
      1)))

(defun my/balance-windows-including-side ()
  "Like `balance-windows', but also equalize left/right side windows."
  (interactive)
  (let* ((frame (selected-frame))
         (side-windows
          (seq-filter (lambda (w)
                        (memq (window-parameter w 'window-side) '(left right)))
                      (window-list frame 'no-minibuffer))))
    (if (null side-windows)
        (balance-windows frame)
      (let* ((columns (+ (my/main-window-columns frame) (length side-windows)))
             (target (floor (window-total-width (frame-root-window frame))
                            columns)))
        (dolist (w side-windows)
          (ignore-errors
            (window-resize w (- target (window-total-width w)) t)))
        ;; Balance ONLY the main area; balancing the whole frame would
        ;; give the side window half again (see comment above).
        (balance-windows (window-main-window frame))))))

(global-set-key (kbd "C-x +") #'my/balance-windows-including-side)

;; --- Manually size / lock / unlock the Claude side window ---
;;
;; Workflow:
;;   1. M-x claude-code-ide-set-window-width   -- set an exact width
;;   2. M-x claude-code-ide-fix-window-width   -- lock it: C-x + (and any
;;      other resize, including mouse dragging) leaves the Claude window
;;      alone and balances only the other windows around it
;;   3. M-x claude-code-ide-unfix-window-width -- unlock: C-x + gives
;;      Claude an equal column share again (the default behavior)
;;
;; The lock uses the built-in buffer-local `window-size-fixed', which
;; every Emacs resize primitive honors.  It lives on the Claude BUFFER,
;; so it survives hiding/re-showing the side window.

(defun claude-code-ide--side-window ()
  "Return the Claude Code side window in the selected frame, or nil."
  (seq-find (lambda (w)
              (and (window-parameter w 'window-side)
                   (string-prefix-p "*claude-code["
                                    (buffer-name (window-buffer w)))))
            (window-list nil 'no-minibuffer)))

(defun claude-code-ide-set-window-width (width)
  "Set the Claude Code side window to WIDTH columns.
Works even while the width is locked with
`claude-code-ide-fix-window-width' (the lock is bypassed for this
one resize and stays on afterwards)."
  (interactive
   (let ((win (or (claude-code-ide--side-window)
                  (user-error "No Claude side window in this frame"))))
     (list (read-number "Claude window width (columns): "
                        (window-total-width win)))))
  (let ((win (or (claude-code-ide--side-window)
                 (user-error "No Claude side window in this frame"))))
    (with-current-buffer (window-buffer win)
      (let ((window-size-fixed nil))
        (window-resize win (- width (window-total-width win)) t)))
    (message "Claude window width set to %d columns" (window-total-width win))))

(defun claude-code-ide-fix-window-width ()
  "Lock the Claude side window at its current width.
`C-x +' then balances only the other windows around it."
  (interactive)
  (let ((win (or (claude-code-ide--side-window)
                 (user-error "No Claude side window in this frame"))))
    (with-current-buffer (window-buffer win)
      (setq-local window-size-fixed 'width))
    (message "Claude window locked at %d columns" (window-total-width win))))

(defun claude-code-ide-unfix-window-width ()
  "Unlock the Claude side window width.
`C-x +' includes it in balancing again (equal column share)."
  (interactive)
  (let ((win (or (claude-code-ide--side-window)
                 (user-error "No Claude side window in this frame"))))
    (with-current-buffer (window-buffer win)
      (setq-local window-size-fixed nil))
    (message "Claude window width unlocked")))

;; --- Fix half-window rendering in Claude's transcript view (C-o) ---
;;
;; claude-code-ide replaces eat's scroll-sync function with its own
;; "position keeper" (`claude-code-ide-eat-preserve-position', default
;; t) to stop the view jumping when you switch windows.  But when the
;; terminal cursor is off-screen it calls plain `recenter', which puts
;; the cursor's line in the MIDDLE of the window.  Claude's transcript
;; view (C-o) parks the cursor on its status bar, so the bar lands
;; mid-window with dead space below while scrolling with j/k.
;;
;; This override keeps the package's behavior (skip in read-only
;; navigation mode; don't touch the view while the cursor is visible
;; mid-buffer) but realigns using eat's stock math -- cursor placed on
;; its actual terminal row, terminal screen pinned to the window -- so
;; full-screen views render full-height.

(defun claude-code-ide--terminal-position-keeper-fixed (window-list)
  "Sync terminal scroll position for WINDOW-LIST, TUI-safe.
Replacement for `claude-code-ide--terminal-position-keeper' that
realigns like eat's stock `eat--synchronize-scroll' instead of
centering the cursor."
  (dolist (win window-list)
    (if (eq win 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      (unless buffer-read-only        ; skip in navigation/scrollback mode
        (let ((cursor (eat-term-display-cursor eat-terminal)))
          (set-window-point win cursor)
          (when (or (>= cursor (- (point-max) 2))
                    (not (pos-visible-in-window-p cursor win)))
            (with-selected-window win
              (recenter
               (- (how-many "\n"
                            (eat-term-display-beginning eat-terminal)
                            cursor)
                  (cdr (eat-term-size eat-terminal))
                  (max 0 (- (floor (window-screen-lines))
                            (cdr (eat-term-size eat-terminal)))))))))))))

(with-eval-after-load 'claude-code-ide
  (advice-add 'claude-code-ide--terminal-position-keeper
              :override #'claude-code-ide--terminal-position-keeper-fixed))

;; ============================================================
;; 11a. Python (Pyright + Ruff)
;; TODO: add venv restart along with lsp restart on project switch
;; ============================================================

(with-eval-after-load 'python
  ;; Jump between blocks at same indentation.
  (define-key python-ts-mode-map (kbd "M-n") #'python-nav-forward-block)
  (define-key python-ts-mode-map (kbd "M-p") #'python-nav-backward-block)
  ;; Jump up/down indentation levels.
  (define-key python-ts-mode-map (kbd "M-u") #'python-nav-backward-up-list)
  (define-key python-ts-mode-map (kbd "M-d") #'python-nav-forward-statement))

(defun my/lsp-pyright-locate-python-from-pyvenv ()
  "Return Python executable from active pyvenv virtualenv."
  (when (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
    (let ((python (expand-file-name "bin/python" pyvenv-virtual-env)))
      (when (file-executable-p python) python))))

(defun my/python-lsp-format-on-save ()
  "Format buffer using LSP and organize imports using Pyright."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (lsp-format-buffer)))

(defun my/python-lsp-setup ()
  "Enable auto-format and auto-import on save for Python."
  (add-hook 'before-save-hook #'my/python-lsp-format-on-save nil t))

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :custom
  (lsp-pyright-modes '(python-mode python-ts-mode))
  (lsp-pyright-type-checking-mode "basic")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-auto-detect-venv t)
  :hook ((python-mode    . my/python-lsp-setup)
         (python-ts-mode . my/python-lsp-setup))
  :config
  (with-eval-after-load 'pyvenv
    (add-to-list 'lsp-pyright-python-search-functions
                 #'my/lsp-pyright-locate-python-from-pyvenv)))

;; Auto-activate tracked virtualenvs.
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-tracking-mode 1))

;; --- Ruff: auto-fix + organize imports on save ---
(with-eval-after-load 'lsp-ruff
  (setq lsp-ruff-major-modes '(python-mode python-ts-mode)))

(defvar my/ruff-allowed-kinds
  '("source.fixAll.ruff"
    "source.organizeImports.ruff")
  "List of Ruff code action kinds that should auto-run on save.")

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

(defun my/python-ruff-setup ()
  "Run approved Ruff code actions before saving."
  (add-hook 'before-save-hook #'my/ruff-apply-actions nil t))

(add-hook 'python-mode-hook    #'my/python-ruff-setup)
(add-hook 'python-ts-mode-hook #'my/python-ruff-setup)

;; --- Ruff linting via Flymake ---
(use-package flymake-ruff
  :ensure t
  :hook ((python-mode    . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))

;; ============================================================
;; 11b. Go (gopls)
;; ============================================================

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

;; ============================================================
;; 11c. Rust (rust-analyzer)
;; ============================================================

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
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

(with-eval-after-load 'lsp-mode
  (add-hook 'rust-ts-mode-hook #'lsp-semantic-tokens-mode))

;; ============================================================
;; 11d. Clojure (clojure-ts-mode + CIDER + clojure-lsp)
;; ============================================================
;;
;; Prelude's clojure module provides clojure-mode + CIDER; on top of
;; that, clojure-ts-mode gives tree-sitter highlighting/indentation
;; (CIDER and lsp-mode both support it natively), and clojure-lsp adds
;; static analysis (find refs, rename, clean-ns) while CIDER owns the
;; REPL.
;;
;; Install the language server with:
;;   brew install clojure-lsp/brew/clojure-lsp-native
;; LSP only activates when the binary is found, so nothing breaks
;; before you install it.

(use-package cider
  :ensure t
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-save-file-on-load t)
  (cider-repl-history-file
   (expand-file-name "cider-repl-history" user-emacs-directory))
  (cider-eldoc-display-context-dependent-info t)
  :config
  ;; CIDER owns M-. / M-, in its buffers when the REPL is connected;
  ;; clojure-lsp serves them when it isn't.
  (setq cider-font-lock-dynamically '(macro core function var)))

(defun my/clojure-lsp-maybe-start ()
  "Start clojure-lsp when the server binary is installed."
  (when (executable-find "clojure-lsp")
    (lsp-deferred)))

;; Classic clojure-mode hooks kept as a fallback (used if the
;; tree-sitter grammar is ever unavailable).
(dolist (hook '(clojure-mode-hook
                clojurescript-mode-hook
                clojurec-mode-hook))
  (add-hook hook #'my/clojure-lsp-maybe-start))

(defun my/clojure-ts-prelude-defaults ()
  "Apply Prelude's clojure/lisp defaults in clojure-ts buffers.
clojure-ts-mode derives from prog-mode, not clojure-mode, so
Prelude's clojure hooks never fire there on their own."
  (subword-mode +1)
  ;; smartparens-strict-mode + rainbow-delimiters-mode
  (run-hooks 'prelude-lisp-coding-hook))

(use-package clojure-ts-mode
  :ensure t
  :init
  ;; Explicit remap: the ts mode functions are autoloaded, so this
  ;; pulls the package in lazily on the first .clj/.cljs/.cljc/.edn
  ;; file (the package's own auto-remap only applies once it is
  ;; already loaded).
  (dolist (entry '((clojure-mode       . clojure-ts-mode)
                   (clojurescript-mode . clojure-ts-clojurescript-mode)
                   (clojurec-mode      . clojure-ts-clojurec-mode)))
    (add-to-list 'major-mode-remap-alist entry))
  :custom
  ;; cljfmt / community style-guide indentation (default, kept explicit)
  (clojure-ts-indent-style 'semantic)
  ;; C-M-x on forms inside (comment ...) evals the inner form
  (clojure-ts-toplevel-inside-comment-form t)
  ;; Grammars (clojure + markdown-inline for docstrings + regex)
  ;; auto-install into ~/.emacs.d/tree-sitter/ on first activation
  ;; (needs git + a C compiler); clojure-ts-ensure-grammars defaults
  ;; to t.
  :hook
  ;; clojure-ts-clojurescript/clojurec modes derive from
  ;; clojure-ts-mode, so one hook covers all three.
  ((clojure-ts-mode . my/clojure-ts-prelude-defaults)
   (clojure-ts-mode . my/clojure-lsp-maybe-start)))

;; ============================================================
;; 11e. Elixir (ElixirLS)
;; ============================================================

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

(use-package elixir-ts-mode
  :mode (("\\.ex\\'"  . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode))
  :hook
  (elixir-ts-mode . lsp-deferred)
  (elixir-ts-mode . my/elixir-lsp-setup)
  :config
  (setq lsp-elixir-fetch-deps t)
  (setq lsp-elixir-dialyzer-enabled t))

;; ============================================================
;; 11f. Node.js / TypeScript (vtsls)
;; ============================================================

(setq lsp-clients-typescript-tls-path
      (or (executable-find "vtsls") "vtsls"))

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

;; typescript-ts-mode.el defines both typescript-ts-mode and tsx-ts-mode.
(use-package typescript-ts-mode
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook
  ((typescript-ts-mode tsx-ts-mode) . lsp-deferred)
  ((typescript-ts-mode tsx-ts-mode) . my/ts-lsp-setup))

;; JS (already remapped to js-ts-mode, just ensure hooks).
(add-hook 'js-ts-mode-hook #'lsp-deferred)
(add-hook 'js-ts-mode-hook #'my/ts-lsp-setup)

;; --- ESLint (built into lsp-mode) ---
(with-eval-after-load 'lsp-mode
  (setq lsp-eslint-enable t)
  (setq lsp-eslint-format t)
  (setq lsp-eslint-run "onType"))

(defun my/eslint-apply-fixes ()
  "Apply ESLint fixes on save."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/codeAction"))
    (lsp-execute-code-action-by-kind "source.fixAll.eslint")))

(defun my/ts-eslint-setup ()
  "Run ESLint fixes before saving."
  (add-hook 'before-save-hook #'my/eslint-apply-fixes nil t))

(dolist (hook '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook))
  (add-hook hook #'my/ts-eslint-setup))

;; --- vtsls tuning (lsp-javascript knobs) ---
(setq lsp-javascript-suggest-auto-imports t)
(setq lsp-typescript-suggest-auto-imports t)
(setq lsp-clients-typescript-max-ts-server-memory 4096)
(setq lsp-javascript-completions-complete-function-calls t)

;; ============================================================
;; 11g. Terraform / HCL
;; ============================================================

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\'"     . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode)))

(use-package hcl-mode
  :ensure t
  :mode (("\\.hcl\\'" . hcl-mode)))

;; Prefer tree-sitter modes if they exist (won't error otherwise).
(with-eval-after-load 'treesit
  (when (fboundp 'terraform-ts-mode)
    (add-to-list 'major-mode-remap-alist '(terraform-mode . terraform-ts-mode)))
  (when (fboundp 'hcl-ts-mode)
    (add-to-list 'major-mode-remap-alist '(hcl-mode . hcl-ts-mode))))

(defun my/terraform-hcl-lsp-format-on-save ()
  "Format current buffer via LSP (Terraform/HCL)."
  (when (and (bound-and-true-p lsp-mode)
             (derived-mode-p 'terraform-mode 'hcl-mode
                             'terraform-ts-mode 'hcl-ts-mode))
    (lsp-format-buffer)))

(defun my/terraform-hcl-setup ()
  "Start LSP and enable format-on-save for Terraform/HCL buffers."
  (lsp-deferred)
  (add-hook 'before-save-hook #'my/terraform-hcl-lsp-format-on-save nil t))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))
  (add-to-list 'lsp-language-id-configuration '(hcl-mode       . "hcl"))
  (when (fboundp 'terraform-ts-mode)
    (add-to-list 'lsp-language-id-configuration '(terraform-ts-mode . "terraform")))
  (when (fboundp 'hcl-ts-mode)
    (add-to-list 'lsp-language-id-configuration '(hcl-ts-mode . "hcl"))))

(dolist (hook '(terraform-mode-hook hcl-mode-hook))
  (add-hook hook #'my/terraform-hcl-setup))
(when (fboundp 'terraform-ts-mode)
  (add-hook 'terraform-ts-mode-hook #'my/terraform-hcl-setup))
(when (fboundp 'hcl-ts-mode)
  (add-hook 'hcl-ts-mode-hook #'my/terraform-hcl-setup))

;; ============================================================
;; 12. Theme (kept last: everything it themes is configured above)
;; ============================================================

(use-package doom-themes
  :ensure t
  :init
  ;; Load after init so faces aren't partially set up.  Under the
  ;; daemon this runs before the first frame exists; the frame hook
  ;; in section 4 re-applies the cursor/region colors per frame.
  (add-hook 'after-init-hook
            (lambda ()
              (load-theme 'doom-dark+ t)))
  :config
  (setq doom-themes-treesitter-colored-indent-levels t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))

(provide 'kanishk-conf)
;;; kanishk-conf.el ends here
