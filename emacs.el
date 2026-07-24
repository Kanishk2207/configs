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
;; /opt/homebrew/bin is needed both for `exec-path' lookups and for
;; vterm's first-load compile (it shells out to `cmake', which reads the
;; PATH env var, not `exec-path'), so keep both in sync.
(dolist (dir '("/Users/kanishk/elixir-ls/release"
               "/Users/kanishk/.nvm/versions/node/v24.14.0/bin"
               "/opt/homebrew/bin"))
  (add-to-list 'exec-path dir))

(let ((brew-bin "/opt/homebrew/bin"))
  (unless (member brew-bin (split-string (or (getenv "PATH") "") path-separator))
    (setenv "PATH" (concat brew-bin path-separator (getenv "PATH")))))

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
        (elixir-mode . elixir-ts-mode)
        (js-json-mode . json-ts-mode)))

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

;; Clipboard from terminal (tty) Emacs clients.  GUI frames already
;; reach the macOS clipboard natively; tty clients don't -- clipetty
;; sends kills to the terminal via OSC 52 so copies (including vterm's
;; copy-mode) land on the system clipboard.  It only acts in terminal
;; frames, so it's a no-op in GUI.  Requires the terminal emulator to
;; allow OSC 52 clipboard writes:
;;   - iTerm2: Settings > General > Selection >
;;             "Applications in terminal may access clipboard"
;;   - tmux:   set -g set-clipboard on
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

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
   (rust-ts-mode   . lsp)
   (json-ts-mode   . lsp))
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
                   (js-ts-mode         . "javascript")
                   (json-ts-mode       . "json")))
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

;; Pull Claude's on-disk edits into open buffers promptly and without
;; prompting, so the hand-off is smooth.  `global-auto-revert-mode' is
;; already on (Prelude, prelude-editor.el), but the defaults poll every
;; 5s.  Note: Emacs still refuses to revert a buffer with UNSAVED
;; changes -- that is the real "ownership fight".  Save your buffers
;; before letting Claude edit the same file.
(with-eval-after-load 'autorevert
  (setq auto-revert-use-notify t)     ;; file-system notifications, not polling
  (setq auto-revert-interval 1)       ;; fallback poll: 1s instead of 5s
  (setq auto-revert-verbose nil)
  (setq revert-without-query '(".*"))) ;; never prompt on an unmodified buffer

;; Terminal backend for claude-code-ide.  vterm renders Claude's
;; full-screen TUI far more reliably than eat (no overlapping text) and
;; the mouse behaves.  It compiles a native module on first load
;; (cmake + libvterm, installed via Homebrew); this works under the
;; daemon once built.  `eat' is kept installed as a pure-elisp fallback.
(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000)
  ;; How narrow the terminal may get before vterm stops shrinking it.
  ;; vterm clamps the width it reports to the child to at least this many
  ;; columns (see `vterm--window-adjust-process-window-size':
  ;; `(max width vterm-min-window-width)').  The default is 80, so once the
  ;; Claude window is narrower than 80 columns Claude keeps wrapping its
  ;; output at 80 while the window is narrower -- and because vterm buffers
  ;; use `truncate-lines', those over-wide lines get clipped at the right
  ;; edge instead of wrapping (the "text leaks out of the window" bug).
  ;; Lower the floor to 20 so the terminal keeps following the real window
  ;; width down to 20 columns, wrapping to fit instead of leaking.
  (vterm-min-window-width 20)
  :config
  ;; tmux-style: `C-c [' enters copy mode -- a read-only Emacs view over
  ;; the terminal and its scrollback.  In copy mode every normal Emacs
  ;; motion key works as-is (C-a/C-e line ends, C-p/C-n/C-f/C-b,
  ;; C-v/M-v page, C-s search).  Press `C-c [' again (or `q') to leave.
  ;;
  ;; In a Claude buffer, plain `C-c [' first dumps the whole conversation
  ;; into scrollback so copy mode can reach the FULL history, not just the
  ;; current screen (Claude's fullscreen TUI keeps history off-buffer; see
  ;; `my/vterm-copy-or-history').  Use `C-u C-c [' to skip the dump and
  ;; copy just the visible screen (instant).  Ordinary vterm shells are
  ;; unaffected -- there `C-c [' is plain copy mode as before.
  ;;
  ;; NOTE: we deliberately do NOT use `C-SPC ['.  vterm binds C-SPC to
  ;; self-insert, tmux uses C-Space as its prefix in tty clients, and
  ;; macOS often grabs Ctrl+Space for input-source switching in GUI --
  ;; so C-SPC is unreliable everywhere.  `C-c' is in
  ;; `vterm-keymap-exceptions', so it always reaches Emacs.
  (define-key vterm-mode-map      (kbd "C-c [") #'my/vterm-copy-or-history)
  (define-key vterm-copy-mode-map (kbd "C-c [") #'vterm-copy-mode)
  ;; In a full-screen TUI like Claude, line editing belongs to the app,
  ;; not to vterm's shell-oriented C-a/C-e (which move the Emacs point to
  ;; the buffer-line ends; in Claude that's the input-box border).  We
  ;; send the dedicated Home/End keys rather than raw ^A/^E: Claude's TUI
  ;; input reliably honors Home/End, whereas raw ^E after ^A was being
  ;; dropped.  `vterm-send' translates the named key into the right
  ;; terminal escape sequence.  Copy mode keeps vterm's C-a/C-e for
  ;; buffer navigation, so we only rebind vterm-mode-map.
  (define-key vterm-mode-map (kbd "C-a")
              (lambda () (interactive) (vterm-send "<home>")))
  (define-key vterm-mode-map (kbd "C-e")
              (lambda () (interactive) (vterm-send "<end>")))
  ;; C-l: in a Claude buffer, a throttled redraw that can never become
  ;; `/clear' no matter how often you press it; in an ordinary shell, the
  ;; usual terminal C-l.  See `my/vterm-ctrl-l' / `my/claude-vterm-redraw'.
  (define-key vterm-mode-map (kbd "C-l") #'my/vterm-ctrl-l)
  ;; C-c C-e: edit the program's current input line in a real Emacs buffer.
  ;; In a Claude buffer this sends Claude's `Ctrl-g' ("edit in editor"): Claude
  ;; writes the prompt to a temp file, opens it with $VISUAL (our
  ;; `personal/claude-emacsclient', which routes it back into THIS Emacs), and
  ;; on `C-x #' replaces the input with what you saved.  That is the reliable
  ;; way to select / kill / yank / rewrite arbitrary text in the prompt --
  ;; unlike walking the terminal cursor with arrow keys, which loses races
  ;; against Claude's constantly-repainting TUI.  We bind it here because
  ;; Emacs itself swallows a real C-g (`keyboard-quit'), so it never reaches
  ;; the program on its own.  See `my/vterm-edit-input-in-editor'.
  (define-key vterm-mode-map (kbd "C-c C-e") #'my/vterm-edit-input-in-editor))

;; --- Edit the program's input line in a real Emacs buffer ---
;;
;; Why not just select+delete in place?  A vterm selection is only an Emacs
;; overlay; the text belongs to the child program (Claude, or a shell), and
;; Emacs can only *send keystrokes*.  Deleting an arbitrary middle chunk means
;; walking the program's cursor onto the selection with arrow-key sends and
;; verifying each step against the rendered screen -- and that loses races
;; against Claude's Ink TUI, which repaints the whole screen every frame (an
;; arrow escape can get echoed as literal `^[[C', or a repaint can land
;; mid-move).  So instead of fighting the terminal, hand the whole line to
;; Emacs: Claude Code binds `Ctrl-g' (also `Ctrl-x Ctrl-e') to "edit in
;; editor" -- it dumps the current prompt to a temp file, blocks on $VISUAL,
;; and on exit replaces the input with the file's contents.  With $VISUAL set
;; to `personal/claude-emacsclient' the file opens right here (see the advice
;; on the session creator below), giving full Emacs editing -- region select,
;; kill/yank, undo, multiple cursors -- with zero raciness.  Finish with the
;; standard `C-x #' (server-edit) and Claude picks up your edit.
(defun my/vterm-edit-input-in-editor ()
  "Open the terminal program's current input in Emacs by sending `Ctrl-g'.
Meant for a Claude buffer, where Ctrl-g is \"edit in editor\"; in a plain
shell it is a harmless interrupt/bell.  We send Ctrl-g through the PTY
because Emacs binds C-g to `keyboard-quit' and would otherwise eat it."
  (interactive)
  (let ((proc (and (boundp 'vterm--process) vterm--process)))
    (unless (and proc (process-live-p proc))
      (user-error "No live terminal process in this buffer"))
    (vterm-send-key "g" nil nil t)      ; Ctrl-g
    (message "Editing the prompt in Emacs -- C-c C-e to confirm, C-g to cancel")))

;; Prelude's minor mode (`prelude-mode') rebinds C-a (to
;; crux-move-beginning-of-line) and M-o.  Minor-mode maps outrank the
;; major-mode map, so in vterm those shadow vterm's own C-a/C-e -- C-a
;; was running crux (moving the Emacs point) instead of reaching the
;; shell, which is why nothing bound to C-a in vterm-mode-map ever fired.
;; In a terminal we want keys to pass through, so neutralize
;; prelude-mode's bindings buffer-locally with an empty override keymap
;; (`minor-mode-overriding-map-alist' is automatically buffer-local),
;; letting vterm-mode-map win.
;; --- Mouse wheel / touchpad scrolling for Claude's full-screen TUI ---
;;
;; Claude Code runs in its "fullscreen" renderer (see `/tui fullscreen'):
;; it draws on the terminal's ALTERNATE screen, like vim, so the
;; conversation lives in Claude's own render state, NOT in vterm's buffer
;; -- the buffer only ever holds the current screen.  So scrolling the
;; Emacs buffer does nothing useful; you have to make Claude scroll.
;; Claude scrolls its conversation when it receives mouse-wheel events,
;; but vterm never forwards mouse events to the child program.  So do it
;; ourselves: translate an Emacs wheel event into the SGR mouse escape a
;; real terminal would send and write it straight down the PTY.  Claude
;; then scrolls a few lines per notch, with its own acceleration and
;; `CLAUDE_CODE_SCROLL_SPEED' (run `/scroll-speed' or set the env var; use
;; a value < 1 to tame a fast trackpad).
(defun my/vterm-wheel-scroll (event)
  "Forward mouse-wheel EVENT to the terminal program as an SGR mouse event.
In `vterm-copy-mode' (a read-only Emacs view) scroll the buffer normally
instead, since there we are navigating Emacs, not the live program."
  (interactive "e")
  (if (bound-and-true-p vterm-copy-mode)
      ;; Copy mode is a read-only Emacs view over the buffer, so scroll the
      ;; buffer normally -- smoothly, and without erroring at the edges.
      ;; NOTE: in Claude's fullscreen mode the buffer holds ONLY the current
      ;; screen, so there is nothing to scroll here until you pull the
      ;; transcript into it: press `C-o' then `[' in Claude to dump the whole
      ;; conversation into the buffer, after which this scrolls all of it.
      (ignore-errors
        (if (fboundp 'pixel-scroll-precision)
            (pixel-scroll-precision event)
          (pcase (event-basic-type event)
            ('wheel-up   (scroll-down-command 3))
            ('wheel-down (scroll-up-command 3)))))
    (let ((proc (and (boundp 'vterm--process) vterm--process)))
      (when (and proc (process-live-p proc))
        (let ((btn (pcase (event-basic-type event)
                     ('wheel-up 64) ('wheel-down 65)
                     ('wheel-left 66) ('wheel-right 67)))
              (cr (posn-col-row (event-start event))))
          (when btn
            ;; SGR mouse press: ESC [ < Btn ; Col ; Row M  (1-based coords).
            (process-send-string
             proc (format "\e[<%d;%d;%dM" btn (1+ (car cr)) (1+ (cdr cr))))))))))

(defvar my/vterm-wheel-map
  (let ((m (make-sparse-keymap)))
    (dolist (k '([wheel-up] [wheel-down] [wheel-left] [wheel-right]))
      (define-key m k #'my/vterm-wheel-scroll))
    m)
  "Keymap that forwards wheel events to the terminal program.")

;; --- Copy from Claude's off-screen history ---
;;
;; Because Claude's fullscreen TUI keeps the conversation on the alternate
;; screen, vterm's buffer holds only the current screen and copy mode has
;; nothing above it to reach.  Claude's transcript mode can write the whole
;; conversation into the terminal as ordinary text (`Ctrl-o' then `['), which
;; DOES populate vterm's scrollback.  `my/claude-dump-then-copy' automates
;; that and drops into copy mode so you can scroll and select the full
;; history.  Caveat: the render streams the entire conversation through
;; vterm, so on very long conversations it can pause Emacs for a few
;; seconds.  A lighter route is Claude's `v' (transcript -> temp file ->
;; $EDITOR); set EDITOR/VISUAL to emacsclient and it opens straight in
;; Emacs without the vterm render.
(defun my/claude-dump-then-copy ()
  "Write Claude's full conversation into vterm scrollback, then enter copy mode."
  (let ((proc (and (boundp 'vterm--process) vterm--process)))
    (unless (and proc (process-live-p proc))
      (user-error "No live Claude process in this buffer"))
    (when (fboundp 'vterm-clear-scrollback)
      (ignore-errors (vterm-clear-scrollback)))  ; avoid stacking old dumps
    (message "Claude: writing conversation to scrollback...")
    (let ((base (buffer-size)))
      (process-send-string proc "\C-o")          ; enter transcript mode
      (accept-process-output proc 0.4)
      (process-send-string proc "[")             ; dump conversation to scrollback
      ;; Wait for the dump to START (buffer grows past baseline) BEFORE
      ;; watching for it to finish.  Skipping the start wait was the bug: on
      ;; any render latency the "stopped growing" test passed instantly, so we
      ;; entered copy mode on a stale screen while the dump was still arriving.
      (let ((n 0))
        (while (and (< (buffer-size) (+ base 200)) (< n 30))
          (accept-process-output proc 0.1)
          (setq n (1+ n))))
      ;; Now wait for it to FINISH: buffer size stable for ~0.8s.
      (let ((last -1) (idle 0))
        (while (and (< idle 5) (< (buffer-size) 20000000))
          (accept-process-output proc 0.16)
          (setq idle (if (= (buffer-size) last) (1+ idle) 0)
                last (buffer-size)))))
    (process-send-string proc "q")               ; back to the live screen
    (accept-process-output proc 0.3)
    (sleep-for 0.15)                             ; let the alt-screen finish repainting
    (vterm-copy-mode 1)
    ;; Keep the view exactly as it was: pin the live screen to the bottom of
    ;; the window (so nothing appears to move), with the dumped history just
    ;; above it -- scroll up to reach it.
    (goto-char (point-max))
    (when (get-buffer-window) (recenter -1))
    (message "Claude: history in scrollback -- scroll up (M-v) to reach it, C-c [ to exit")))

(defun my/vterm-copy-or-history (&optional arg)
  "Enter `vterm-copy-mode'.
In a Claude buffer, first dump the whole conversation into scrollback so
copy mode can reach the full history (see `my/claude-dump-then-copy').
With prefix ARG, or in an ordinary vterm shell, skip the dump and just
toggle copy mode on the current screen."
  (interactive "P")
  (if (and (not arg)
           (not (bound-and-true-p vterm-copy-mode))
           (string-match-p "claude-code" (buffer-name)))
      (my/claude-dump-then-copy)
    (vterm-copy-mode 'toggle)))

(defun my/vterm-let-terminal-own-keys ()
  "Stop `prelude-mode' from shadowing vterm's own keybindings.
Also apply a small redisplay-cost win for the terminal buffer."
  (push (cons 'prelude-mode (make-sparse-keymap))
        minor-mode-overriding-map-alist)
  ;; Select-to-copy, like a normal terminal emulator: dragging out a
  ;; selection in a vterm buffer copies it to the kill ring immediately --
  ;; no `M-w' needed.  From the kill ring it reaches the system clipboard the
  ;; usual way: natively on a GUI frame, and via clipetty's OSC 52 on a tty
  ;; (the same path copy-mode's `M-w' already uses; needs `set -g
  ;; set-clipboard on' in tmux and an OSC-52-capable terminal).  Buffer-local
  ;; so it changes mouse behavior only inside terminals, never in your
  ;; editing buffers.
  (setq-local mouse-drag-copy-region t)
  ;; Small redisplay-cost win: terminal output is left-to-right and never
  ;; needs Emacs's bidirectional reordering engine (one of redisplay's
  ;; bigger per-line costs), so pin the paragraph direction and skip bidi
  ;; parenthesis analysis.
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t)
  ;; vterm stores its ANSI colors in the `font-lock-face' text property,
  ;; which the display engine paints as a real face ONLY when `face' is
  ;; aliased to `font-lock-face' -- an alias `font-lock-mode' normally
  ;; installs.  We keep font-lock OFF in vterm for performance (see
  ;; `my/vterm-disable-heavy-modes'), which removed that alias and left the
  ;; terminal colorless even though the color properties were still there.
  ;; So install the alias ourselves: this renders Claude's full color with
  ;; none of font-lock's per-change overhead.
  (setq-local char-property-alias-alist
              (cons '(face font-lock-face)
                    (default-value 'char-property-alias-alist)))
  ;; NOTE: we used to throttle `vterm-timer-delay' to 0.2 here to stop
  ;; Claude's constantly-repainting TUI from starving global redisplay.
  ;; That was a band-aid: the real cause of the expensive redraws was
  ;; heavy per-change minor modes running in the vterm buffer (font-lock,
  ;; Flycheck, yascroll -- see `my/vterm-disable-heavy-modes').  With those
  ;; excluded each redraw is cheap again, so we leave `vterm-timer-delay'
  ;; at vterm's default (0.1s), which streams Claude's text smoothly.
  (when (string-match-p "claude-code" (buffer-name))
    ;; Forward the mouse wheel / touchpad to Claude (see
    ;; `my/vterm-wheel-scroll').  Override BOTH the pixel-scroll and classic
    ;; mouse-wheel minor modes, which grab wheel events at higher precedence
    ;; than the major-mode map.  Only for Claude buffers: a plain vterm shell
    ;; has no mouse mode, so there the wheel should keep scrolling the buffer.
    (dolist (mode '(pixel-scroll-precision-mode mouse-wheel-mode))
      (push (cons mode my/vterm-wheel-map) minor-mode-overriding-map-alist))
    ;; Snap this window to the live screen when it is re-selected (see
    ;; `my/claude-vterm-snap-to-bottom').  Registered BUFFER-LOCALLY (the
    ;; trailing t) so it fires only for the Claude window's own selection
    ;; changes -- a global hook here would run on every window switch in
    ;; the frame.
    (add-hook 'window-selection-change-functions
              #'my/claude-vterm-snap-to-bottom nil t)
    ;; Repaint on every switch back, so a prompt bar left distorted while we
    ;; were away is always cleared; see `my/claude-vterm-redraw-on-select'.
    (add-hook 'window-selection-change-functions
              #'my/claude-vterm-redraw-on-select nil t)))
(add-hook 'vterm-mode-hook #'my/vterm-let-terminal-own-keys)

;; Keep `global-display-line-numbers-mode' (enabled by Prelude in
;; prelude-ui.el) out of terminal buffers.  Line numbers are useless in a
;; terminal and, worse, vterm reserves a left margin for the gutter and
;; subtracts it from the PTY width it reports to the child -- with a 10000
;; line scrollback that is 9 columns, which is the blank strip on the
;; right of Claude's full-screen TUI.  The global enabler runs on
;; `after-change-major-mode-hook', which fires *after* `vterm-mode-hook',
;; so disabling in the mode hook gets clobbered; we append to the same
;; hook instead so our disable runs last and wins.
(defun my/vterm-no-line-numbers ()
  "Turn off `display-line-numbers-mode' in vterm buffers."
  (when (derived-mode-p 'vterm-mode)
    (display-line-numbers-mode -1)))
(add-hook 'after-change-major-mode-hook #'my/vterm-no-line-numbers t)

;; Keep heavy global minor modes out of terminal buffers -- the single
;; biggest cause of Claude-window lag.  vterm rewrites its buffer on
;; every terminal repaint and fires `after-change-functions' MANY times
;; per redraw; any globalized minor mode hooked there then runs on each
;; of those edits.  Measured on this config: font-lock
;; (`jit-lock-after-change'), Flycheck (`flycheck-handle-change') and
;; yascroll (`yascroll:after-change') together turned a ~2ms vterm redraw
;; into 60-190ms.  Because Emacs redisplay and the command loop are
;; single-threaded, each such redraw froze the WHOLE UI for that long --
;; so holding C-n or C-v while Claude repainted made the cursor hop in
;; lockstep with its output.  None of these do anything useful in a
;; terminal: there is no code to lint or scroll-map, and font-lock-mode's
;; keyword fontification is meaningless here (vterm's own colors live in
;; the `font-lock-face' property, which we render via a face alias set in
;; `my/vterm-let-terminal-own-keys', NOT via font-lock-mode).  So exclude
;; vterm-mode from the globalized modes.
(setq font-lock-global-modes '(not vterm-mode))
(with-eval-after-load 'flycheck
  (setq flycheck-global-modes '(not vterm-mode)))

;; Catch-all safety net: the globalized-mode enablers run from
;; `after-change-major-mode-hook', so append a disable there (runs last,
;; wins) for anything the exclusion lists above miss -- notably yascroll,
;; which has no exclusion variable.
(defun my/vterm-disable-heavy-modes ()
  "Disable `after-change-functions' minor modes in vterm buffers."
  (when (derived-mode-p 'vterm-mode)
    (when (bound-and-true-p font-lock-mode) (font-lock-mode -1))
    (when (and (fboundp 'flycheck-mode) (bound-and-true-p flycheck-mode))
      (flycheck-mode -1))
    (when (and (fboundp 'yascroll-bar-mode) (bound-and-true-p yascroll-bar-mode))
      (yascroll-bar-mode -1))))
(add-hook 'after-change-major-mode-hook #'my/vterm-disable-heavy-modes t)

;; Snap Claude's window back to the live screen when you return to it.
;; Claude is a full-screen TUI, so the only useful view is the bottom of
;; the buffer (the current screen); the scrollback above it holds stale
;; frames.  claude-code-ide sets `vterm-scroll-to-bottom-on-output' nil
;; and its scroll "position keeper" only runs for the eat backend, not
;; vterm -- so when you switch away, let Claude repaint, then switch back,
;; the window is still showing the old viewport with the freshly redrawn
;; prompt overlapping it (the duplicated prompt bar).  On re-selecting the
;; window, jump to the process output so the live screen is what's shown.
;; Guarded against copy mode (`C-c ['), where you are deliberately
;; scrolled up reading history and must not be yanked to the bottom.
(defun my/claude-vterm-snap-to-bottom (&rest _)
  "Show Claude's live terminal screen when its window is re-selected.
Added buffer-locally to `window-selection-change-functions' in the
Claude vterm buffer (see `my/vterm-let-terminal-own-keys'), so it only
runs for that window, never on ordinary window switches."
  (let ((win (selected-window)))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (when (and (derived-mode-p 'vterm-mode)
                   (not (bound-and-true-p vterm-copy-mode))
                   (string-match-p "claude-code" (buffer-name)))
          (set-window-point win (point-max)))))))

;; --- Repaint Claude whenever you switch back to its window ---
;;
;; Claude's TUI often leaves its bottom-anchored input box drawn at a stale
;; row after you have been away -- the "distorted prompt bar."  One common
;; trigger is a project switch: claude-code-ide suppresses HEIGHT-only
;; reflows (a workaround for a streaming scroll glitch, upstream issue
;; #1422), so when the side window comes back at a different height Claude
;; is never told, keeps drawing its input box at the old row, and leaves
;; stale fragments below it.  Incremental repaints can also settle wrong on
;; their own.  A plain resize signal does not fix it (Claude repaints
;; incrementally and keeps the fragments); a single `C-l' makes Claude
;; re-query the size and FULLY repaint, which clears them.  So on
;; re-selecting the Claude window, always send one `C-l'.
;;
;; The redraw uses `C-l' because it is bound to `chat:clearInput', the ONLY
;; action that does the heavy clear-and-full-repaint that fixes the
;; fragments (the lighter `app:redraw' does not re-query size, so it leaves
;; the stale rows -- verified).  The catch is that `chat:clearInput's
;; DOUBLE press within 2s is Claude's `/clear', and the two behaviors
;; cannot be separated in Claude's settings (rebinding to an odd key does
;; not work either: Claude does not recognize keys like Ctrl+\ from vterm).
;;
;; So we make `C-l' safe from the EMACS side.  `my/vterm-ctrl-l' handles
;; C-l in the terminal (see the vterm `use-package'): in a Claude buffer it
;; routes to `my/claude-vterm-redraw', which sends the raw C-l byte to the
;; process (bypassing the keymap) but THROTTLES to at most once per 2.5s.
;; Since `/clear' needs two presses within 2s, a throttled single send can
;; never trigger it -- mash C-l all you like, it only ever refreshes.  The
;; on-select redraw below shares the same throttled sender, so an automatic
;; redraw and a manual C-l can never combine into `/clear' either.  To
;; actually clear, type `/clear'.
(defvar-local my/claude-vterm-last-redraw 0
  "`float-time' of the last C-l redraw sent to this Claude buffer.")

(defun my/claude-vterm-redraw ()
  "Send one `chat:clearInput' redraw (raw C-l) to Claude, throttled.
At most one send per 2.5s, so it can never be the `C-l' `C-l' within 2s
that Claude reads as `/clear'.  Used for C-l in a Claude buffer and by
`my/claude-vterm-redraw-on-select'."
  (interactive)
  (let ((proc (and (boundp 'vterm--process) vterm--process))
        (now (float-time)))
    (when (and proc (process-live-p proc)
               (> (- now my/claude-vterm-last-redraw) 2.5))
      (setq my/claude-vterm-last-redraw now)
      (process-send-string proc "\C-l"))))

(defun my/vterm-ctrl-l ()
  "C-l in a vterm buffer.
In a Claude buffer, a throttled redraw that can never become `/clear'
\(see `my/claude-vterm-redraw').  In an ordinary shell, the normal
terminal C-l (clear screen)."
  (interactive)
  (if (string-match-p "claude-code" (buffer-name))
      (my/claude-vterm-redraw)
    (vterm-send "C-l")))

(defun my/claude-vterm-redraw-on-select (&rest _)
  "Repaint Claude every time its window is (re-)selected.
Sends one throttled `C-l' redraw (see `my/claude-vterm-redraw') so a
distorted prompt bar is cleared whenever you switch back to Claude --
whatever left it stale (a project-switch resize, an unsettled incremental
repaint, ...).  Added buffer-locally to `window-selection-change-functions',
so it runs only for the Claude window, never on ordinary window switches.
The throttle in `my/claude-vterm-redraw' means rapid switching never spams
Claude and can never pair into the `/clear' double-press."
  (let ((win (selected-window)))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (when (and (derived-mode-p 'vterm-mode)
                   (string-match-p "claude-code" (buffer-name))
                   (not (bound-and-true-p vterm-copy-mode))
                   (bound-and-true-p vterm--process)
                   (process-live-p vterm--process))
          (my/claude-vterm-redraw))))))   ; throttled; safe from /clear

;; --- Keep libvterm's grid height in sync with Claude's PTY height ---
;;
;; THE root cause of the garbled / distorted Claude TUI (overlapping text,
;; stale prompt bar, cleared only by resizing the frame).
;;
;; claude-code-ide's bug-#1422 workaround (`claude-code-ide-prevent-reflow-glitch')
;; advises vterm's window-size adjuster to SUPPRESS height-only reflows -- but
;; the way it suppresses is broken.  It calls the real adjuster FIRST
;; (`vterm--window-adjust-process-window-size' -> `vterm--set-size'), which
;; resizes *libvterm's* internal grid to the new height, and only THEN returns
;; nil.  Emacs pushes a new size down to the child (Claude) via
;; `set-process-window-size' ONLY when the adjuster returns non-nil (see
;; window.el `window--adjust-process-windows').  So on every height-only change
;; libvterm gets resized but Claude is never told: libvterm ends up one height,
;; Claude another.  Claude then paints its full-screen TUI -- bottom-anchored
;; input box, cursor-relative moves -- for its OLD row count into libvterm's
;; NEW grid, so frames land at the wrong rows and overlap.  That is the
;; garbling.
;;
;; Height-only changes fire constantly here: the echo area growing/shrinking a
;; line, a transient minibuffer message, an ediff/diff window opening, a
;; project switch reshowing the side window.  Each one widens the desync, which
;; is why the TUI corrupts "for no reason" (e.g. mid code-generation) and why a
;; real frame resize -- which also changes WIDTH -- is what clears it: a width
;; change makes the adjuster return non-nil, so Claude finally learns the true
;; size and does one clean full repaint.
;;
;; Fix: disable claude-code-ide's broken workaround (see the `:custom' block on
;; `claude-code-ide' below) and install a correct one.  On a height-only change
;; we touch NOTHING -- neither resize libvterm nor tell Claude -- so the two can
;; never disagree and the TUI never garbles.  A genuine width change falls
;; through to the real adjuster, which resizes libvterm and, by returning the
;; size, makes Emacs tell Claude too: one clean repaint.  This keeps the point
;; of the #1422 fix (no height-only reflow is ever pushed to Claude mid-stream)
;; without the size desync.  The `C-l' redraw above is now just a belt-and-
;; suspenders for non-size repaint settling, not the primary cure.
(defvar-local my/claude-last-pty-width nil
  "Last terminal WIDTH propagated to this Claude process, for change detection.")

(defun my/claude-vterm-sync-size (orig-fn &rest args)
  "Around advice for `vterm--window-adjust-process-window-size'.
In a Claude buffer, only let a size change reach libvterm/Claude when the
WIDTH changed; suppress height-only changes so libvterm's grid and Claude's
PTY never drift apart (see the commentary above).  Any other vterm buffer is
adjusted exactly as usual."
  (if (not (string-match-p "claude-code" (buffer-name)))
      (apply orig-fn args)
    ;; Compute the size vterm would apply WITHOUT side effects, using the same
    ;; function vterm itself uses, so we can detect a width change cheaply.
    (let* ((size (ignore-errors
                   (funcall window-adjust-process-window-size-function
                            (nth 0 args) (nth 1 args))))
           (new-w (and (consp size) (car size))))
      (cond
       ;; Copy mode is a read-only Emacs view; never resize (matches vterm).
       ((bound-and-true-p vterm-copy-mode) nil)
       ;; Width changed (or this is the first adjust): resize libvterm AND, by
       ;; returning non-nil from ORIG-FN, let Emacs push the new size to Claude.
       ((and new-w (not (eql new-w my/claude-last-pty-width)))
        (setq-local my/claude-last-pty-width new-w)
        (apply orig-fn args))
       ;; Height-only wobble: leave the grid untouched so both sides agree.
       (t nil)))))

(with-eval-after-load 'vterm
  (advice-add 'vterm--window-adjust-process-window-size
              :around #'my/claude-vterm-sync-size))

;; If a session already installed claude-code-ide's broken reflow filter
;; (e.g. on a live config reload, before the daemon is restarted), drop it --
;; `my/claude-vterm-sync-size' replaces it.  On a fresh daemon the defcustom
;; below keeps it from ever being added.
(with-eval-after-load 'claude-code-ide
  (when (fboundp 'claude-code-ide--terminal-reflow-filter)
    (advice-remove 'vterm--window-adjust-process-window-size
                   #'claude-code-ide--terminal-reflow-filter)))

;; --- Sync libvterm's grid at OPEN, not just Claude's PTY ---
;;
;; The garbled TUI when the Claude window first opens (and a plain `C-l' won't
;; clear it -- only a resize does) is the same size desync as before, on a code
;; path `my/claude-vterm-sync-size' does not see.  When claude-code-ide first
;; displays the Claude buffer it calls `claude-code-ide--sync-terminal-dimensions',
;; which does `set-process-window-size' DIRECTLY -- telling Claude the window
;; size but never calling `vterm--set-size', so libvterm's grid keeps its
;; creation size.  Claude then paints for the window size into libvterm's
;; different grid: overlap.  `C-l' can't fix it because it only asks Claude to
;; repaint into that same mismatched grid; a real resize does fix it because it
;; finally resizes libvterm to match.
;;
;; Fix: wrap that function so, for the vterm backend, it resizes libvterm to the
;; SAME dimensions it is about to hand Claude (keeping the two in lockstep from
;; the first frame), then sends one clean full repaint (`C-l') once Claude has
;; taken the size.  Because libvterm is already correct, that repaint lands
;; clean.
(defun my/claude-sync-terminal-dims-fix (orig-fn buffer window)
  "Around `claude-code-ide--sync-terminal-dimensions'.
For the vterm backend, resize libvterm to the same dimensions Claude is told,
so the grid and Claude agree at open; then schedule one clean redraw."
  (if (and (eq claude-code-ide-terminal-backend 'vterm)
           (buffer-live-p buffer) (window-live-p window))
      (with-current-buffer buffer
        (let ((height (window-body-height window))
              (width  (window-body-width window)))
          (when (and (bound-and-true-p vterm--term)
                     (fboundp 'vterm--set-size)
                     (> height 0) (> width 0)
                     (not (bound-and-true-p vterm-copy-mode)))
            (ignore-errors (vterm--set-size vterm--term height width))
            (setq-local my/claude-last-pty-width width))
          (prog1 (funcall orig-fn buffer window)
            (when-let ((proc (get-buffer-process buffer)))
              (run-at-time 0.3 nil
                           (lambda (p)
                             (when (and (processp p) (process-live-p p))
                               (process-send-string p "\C-l")))
                           proc)))))
    (funcall orig-fn buffer window)))

(with-eval-after-load 'claude-code-ide
  (advice-add 'claude-code-ide--sync-terminal-dimensions
              :around #'my/claude-sync-terminal-dims-fix))

(use-package eat
  :ensure t)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide-insert-at-mentioned)   ;; autoload stub for the wrapper
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("M-i"     . my/claude-add-region-or-tab))
  :custom
  (claude-code-ide-terminal-backend 'vterm)
  (claude-code-ide-window-side 'right)
  ;; Turn OFF claude-code-ide's bug-#1422 reflow workaround.  It resizes
  ;; libvterm's grid on a height-only change but withholds the new size from
  ;; Claude, so libvterm and Claude disagree on the row count and Claude's
  ;; full-screen repaints overlap -- the garbled TUI that only a frame resize
  ;; cleared.  `my/claude-vterm-sync-size' (see above) replaces it with a
  ;; version that keeps the two in lockstep.
  (claude-code-ide-prevent-reflow-glitch nil)
  ;; Keep the Claude window visible during ediff (default t), but make the
  ;; three windows -- diff pane A, diff pane B and Claude -- equal thirds
  ;; instead of Claude eating half.  See `my/claude-ediff-equal-thirds' below,
  ;; which runs on `ediff-startup-hook' after claude-code-ide re-displays the
  ;; Claude side window.  (That side window is pinned to
  ;; `claude-code-ide-window-width' = 100 and, being a side window, is exempt
  ;; from `C-x +' / `balance-windows' -- which is why balancing by hand did
  ;; nothing.)
  ;; (claude-code-ide-no-flicker t)
  ;; Turn OFF claude-code-ide's "smart renderer" anti-flicker path.  With it
  ;; on (the default), claude-code-ide advises `vterm--filter' to detect
  ;; Claude's full-screen repaints (cursor-up ESC[nA + clear-line ESC[K) and
  ;; batch them behind a 5ms timer with redisplay inhibited.  But Claude's TUI
  ;; (Ink) repaints the WHOLE screen every frame with exactly that pattern, so
  ;; the batcher is firing constantly; when an output chunk splits an escape
  ;; sequence or a repaint races the flush (common after a resize, and worse
  ;; now that we read output in 1MB chunks), a frame lands at a stale cursor
  ;; position and gets drawn on top of the previous one -- the overlapping,
  ;; garbled text.  The advice re-reads this flag at runtime, so nil makes it a
  ;; pass-through immediately (no daemon restart needed).  vterm's own renderer
  ;; handles Ink's repaints correctly, which is why we chose vterm over eat.
  (claude-code-ide-vterm-anti-flicker nil)
  :config
  (claude-code-ide-emacs-tools-setup))

;; --- Equal-width windows when Claude opens an ediff ---
;;
;; claude-code-ide shows a diff by (1) deleting side windows, (2) running ediff
;; with `ediff-setup-windows-plain' + `split-window-horizontally' so the two
;; diff panes fill the frame side by side, then (3) on `ediff-startup-hook'
;; re-displaying the Claude buffer as a right side window pinned to
;; `claude-code-ide-window-width' (100).  That leaves Claude at ~half the frame
;; and squeezes both diff panes into the other half.  A side window is exempt
;; from `balance-windows' (`C-x +'), so it cannot be fixed by hand.
;;
;; Fix: once that startup has run, resize the three content windows -- diff
;; pane A, diff pane B and the Claude window -- to an equal third of their
;; combined width.
;;
;; Two subtleties learned the hard way:
;;  * Don't rely on `ediff-control-buffer' or the current buffer: claude-code-ide's
;;    own startup hook runs first and calls `select-window', so by the time we
;;    run the current buffer is Claude's, where `ediff-control-buffer' is nil.
;;    Instead we find the windows by scanning the frame.
;;  * Run DEFERRED (a 0-delay timer) so it lands after ediff and claude-code-ide
;;    have finished arranging windows, otherwise our resize gets overwritten.
(defun my/claude-ediff-windows (&optional frame)
  "Return (CLAUDE-WIN PANE-1 PANE-2) if a Claude ediff is laid out in FRAME.
CLAUDE-WIN is the Claude side window; PANE-1/PANE-2 are the two diff panes
\(non-side windows that are not the ediff control panel).  nil otherwise."
  (let* ((frame (or frame (selected-frame)))
         (wins (window-list frame 'no-mini))
         (wc (seq-find
              (lambda (w)
                (and (window-parameter w 'window-side)
                     (string-match-p "claude-code" (buffer-name (window-buffer w)))))
              wins))
         (panes (seq-filter
                 (lambda (w)
                   (and (not (window-parameter w 'window-side))
                        (not (string-match-p "Ediff Control Panel"
                                             (buffer-name (window-buffer w))))))
                 wins)))
    (when (and (window-live-p wc) (= (length panes) 2))
      (list wc (car panes) (cadr panes)))))

(defun my/claude-ediff-equal-thirds (&optional frame)
  "Make the two ediff diff panes and the Claude side window equal thirds.
No-op unless a Claude ediff is laid out in FRAME (see `my/claude-ediff-windows'),
so ordinary ediff sessions are left alone."
  (interactive)
  (when-let ((wins (my/claude-ediff-windows frame)))
    (let* ((total (apply #'+ (mapcar #'window-total-width wins)))
           (target (/ total 3)))
      ;; Resize the Claude window (at the frame/side level) and the first pane
      ;; (at the A|B content-row level) to a third each; the second pane absorbs
      ;; the remainder (also a third, modulo one column).  Clear any width lock
      ;; and window-parameter guards so the side window can actually move.
      (dolist (w (list (nth 0 wins) (nth 1 wins)))
        (let ((delta (- target (window-total-width w))))
          (when (/= delta 0)
            (with-current-buffer (window-buffer w)
              (let ((window-size-fixed nil)
                    (ignore-window-parameters t))
                (ignore-errors (window-resize w delta t))))))))))

(defun my/claude-ediff-equal-thirds--deferred (&rest _)
  "Schedule `my/claude-ediff-equal-thirds' after window setup settles."
  (run-at-time 0.05 nil #'my/claude-ediff-equal-thirds))

(add-hook 'ediff-startup-hook #'my/claude-ediff-equal-thirds--deferred t)

;; --- Let Claude open files in THIS Emacs ($VISUAL/$EDITOR) ---
;;
;; Claude uses $VISUAL/$EDITOR for two things, and we point both at one
;; wrapper (`personal/claude-emacsclient') that dispatches on the filename:
;;
;;   * Transcript `v' (in transcript mode, `C-o'): writes the whole
;;     conversation to cc-transcript-<ts>.txt and opens it.  The wrapper opens
;;     that NON-BLOCKING in a read-only, `q'-to-quit view (`emacsclient -e' ->
;;     `my/claude-view-transcript'), so the Claude session is not frozen while
;;     you read.  This is the light-weight way to grab off-screen history from
;;     a LONG conversation -- a file open is cheap, unlike the `C-c ['
;;     scrollback dump, which streams every line through vterm.
;;
;;   * Prompt edit `Ctrl-g' (see `my/vterm-edit-input-in-editor', on C-c C-e),
;;     and `/memory' edits: the wrapper opens these BLOCKING and EDITABLE
;;     (plain `emacsclient FILE'), because Claude waits for the editor to exit
;;     and then reads the file back.  You get full Emacs editing of the prompt;
;;     finish with `C-x #'.
;;
;; Scoped to the Claude launch via advice on the session creator, so it
;; only touches the Claude subprocess -- your global EDITOR (git and
;; friends in other buffers) is left alone.  Takes effect on the NEXT
;; Claude session (an already-running one keeps its original environment).
(defun my/claude-view-transcript (file)
  "Open FILE, a Claude transcript dump, in a dedicated read-only view.
Enables `view-mode', so `q' quits the window (and buries the buffer)
like an ag/grep results buffer, while text stays selectable to copy.
Invoked by `personal/claude-emacsclient', which Claude runs for its
transcript-mode `v' command."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (rename-buffer "*claude-transcript*" t)
      (view-mode 1))                    ; read-only; `q' = View-quit
    (pop-to-buffer buf)))

(defvar vterm-environment)   ; declare special so the let below binds dynamically
(defun my/claude-code-ide--use-emacsclient (orig &rest args)
  "Launch Claude with VISUAL/EDITOR pointing at the emacsclient wrapper."
  (let* ((ed (expand-file-name "personal/claude-emacsclient" user-emacs-directory))
         (extra (list (concat "VISUAL=" ed) (concat "EDITOR=" ed)))
         (process-environment (append extra process-environment))
         (vterm-environment (append extra (and (boundp 'vterm-environment)
                                               vterm-environment))))
    (apply orig args)))
(with-eval-after-load 'claude-code-ide
  (advice-add 'claude-code-ide--create-terminal-session
              :around #'my/claude-code-ide--use-emacsclient))

;; --- Prompt-edit UX: own window, C-c C-e to confirm, C-g to cancel ---
;;
;; When Claude's `Ctrl-g' opens the prompt through the wrapper (see above),
;; we want three things beyond a plain `emacsclient FILE':
;;   1. It opens in its OWN window, not by replacing the buffer beside the
;;      Claude side window.
;;   2. `C-c C-e' (the same key that launched it) confirms: save and hand the
;;      text back to Claude -- no need to remember `C-x #'.
;;   3. `C-g' cancels: throw the edits away and restore the prompt Claude
;;      started with.  (With an active region, C-g first just drops the
;;      region, as usual -- so selecting-then-C-g doesn't nuke your edit.)
;;
;; The wrapper can't pass metadata to Emacs, so it TAGS the file first with a
;; non-blocking `emacsclient -e (my/claude-register-prompt-edit ...)' and then
;; opens it blocking.  `server-visit-hook' matches the tag and sets everything
;; up.  Only files the wrapper registered get this treatment, so ordinary
;; emacsclient edits (git commits in other buffers, etc.) are untouched.

(defvar my/claude-prompt-edit-files nil
  "Truenames of files the Claude editor wrapper flagged as prompt edits.
Consumed once by `my/claude-prompt-edit--maybe-setup'.")

(defvar-local my/claude-prompt-edit--original nil
  "The prompt text Claude started with, for `my/claude-prompt-edit-cancel'.")

(defun my/claude-register-prompt-edit (file)
  "Flag FILE as a Claude prompt edit about to be opened by emacsclient.
Called (non-blocking) by `personal/claude-emacsclient' just before it
opens FILE blocking, so `server-visit-hook' can give it the prompt-edit
UX.  Returns nil so the `emacsclient -e' that calls it prints nothing."
  (cl-pushnew (file-truename file) my/claude-prompt-edit-files :test #'equal)
  nil)

(defun my/claude-prompt-edit--finish ()
  "Save the current text back to Claude, then remove the edit window.
Saving first leaves the buffer unmodified, so `server-edit' hands the
file to the waiting client (and kills this temp buffer) without a
\"Save?\" prompt.  Then drop the extra window and hop back to Claude."
  (when (buffer-modified-p) (save-buffer))
  (let ((win (selected-window)))
    (server-edit)                       ; notify the client; kills this buffer
    (when (and (window-live-p win) (not (one-window-p win)))
      (ignore-errors (delete-window win)))
    (let ((cw (seq-find (lambda (w)
                          (string-match-p "claude-code"
                                          (buffer-name (window-buffer w))))
                        (window-list))))
      (when (window-live-p cw) (select-window cw)))))

(defun my/claude-prompt-edit-confirm ()
  "Hand the edited prompt back to Claude (bound to `C-c C-e')."
  (interactive)
  (my/claude-prompt-edit--finish))

(defun my/claude-prompt-edit-cancel ()
  "Discard the edits and restore the prompt Claude started with."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (or my/claude-prompt-edit--original "")))
  (my/claude-prompt-edit--finish)
  (message "Prompt edit cancelled -- original restored"))

(defun my/claude-prompt-edit-cancel-or-quit ()
  "Drop the active region if any (ordinary `C-g'); else cancel the edit."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (my/claude-prompt-edit-cancel)))

(defvar my/claude-prompt-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-e") #'my/claude-prompt-edit-confirm)
    (define-key m (kbd "C-c C-c") #'my/claude-prompt-edit-confirm) ; git-commit muscle memory
    (define-key m (kbd "C-g")     #'my/claude-prompt-edit-cancel-or-quit)
    (define-key m (kbd "C-c C-k") #'my/claude-prompt-edit-cancel)  ; explicit cancel
    m)
  "Keymap for `my/claude-prompt-edit-mode'.")

(define-minor-mode my/claude-prompt-edit-mode
  "Minor mode while editing Claude's prompt in an emacsclient buffer.
\\<my/claude-prompt-edit-mode-map>Finish with \\[my/claude-prompt-edit-confirm]; \
abandon with \\[my/claude-prompt-edit-cancel]."
  :lighter " Claude-Prompt"
  (when my/claude-prompt-edit-mode
    (setq-local header-line-format
                (substitute-command-keys
                 " Editing Claude prompt  \\`C-c C-e' confirm  ·  \\`C-g' cancel"))))

(defun my/claude-prompt-edit--maybe-setup ()
  "Set up the prompt-edit UX if the visited file was registered.
Runs from `server-visit-hook' in the just-visited buffer.  Pre-displays
the buffer in its own window; because `server-window' is nil, the server
then just reuses that window (see `server-switch-buffer') instead of
replacing the buffer next to Claude."
  (when (and buffer-file-name
             (member (file-truename buffer-file-name) my/claude-prompt-edit-files))
    (setq my/claude-prompt-edit-files
          (delete (file-truename buffer-file-name) my/claude-prompt-edit-files))
    (setq my/claude-prompt-edit--original (buffer-string))
    (my/claude-prompt-edit-mode 1)
    ;; Open in a separate window: never the selected (Claude side) window,
    ;; splitting a main window if needed.
    (pop-to-buffer (current-buffer)
                   '((display-buffer-reuse-window
                      display-buffer-pop-up-window
                      display-buffer-use-some-window)
                     (inhibit-same-window . t)))))
(add-hook 'server-visit-hook #'my/claude-prompt-edit--maybe-setup)

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
         (child (window-child main)))
    (if (and child (window-combined-p child t))
        (let ((n 0))
          (while child
            (setq n (1+ n)
                  child (window-next-sibling child)))
          n)
      1)))

(defun my/balance-windows-including-side ()
  "Like `balance-windows', but also equalize left/right side windows.
During a Claude ediff, make the two diff panes and the Claude window equal
thirds and leave the Ediff Control Panel untouched -- plain `balance-windows'
here would equalize the control panel's vertical split and blow it up to
half-height (the mystery window), and would give Claude a full half."
  (interactive)
  (cond
   ;; Claude ediff in progress: three equal columns, control panel left alone.
   ((my/claude-ediff-windows)
    (my/claude-ediff-equal-thirds))
   (t
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
          (balance-windows (window-main-window frame))))))))

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
;; 11g. JSON (vscode-json-language-server)
;; ============================================================
;;
;; Install the language server with:
;;   npm install -g vscode-langservers-extracted
;; It ships the `vscode-json-language-server' binary lsp-mode uses,
;; giving schema-aware completion, hover, and validation (package.json,
;; tsconfig.json, and other known files validate automatically).

(defun my/json-lsp-format-on-save ()
  "Format JSON buffer via LSP."
  (when (derived-mode-p 'json-ts-mode)
    (lsp-format-buffer)))

(defun my/json-lsp-setup ()
  "Enable format-on-save for JSON buffers."
  (add-hook 'before-save-hook #'my/json-lsp-format-on-save nil t))

(add-hook 'json-ts-mode-hook #'my/json-lsp-setup)

;; ============================================================
;; 11h. Terraform / HCL
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
