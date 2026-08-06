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
;;   7.  Completion (Corfu + Orderless + yasnippet + Swiper)
;;   8.  Windows, Dired, Ediff, Magit, Projectile, Flycheck
;;   9.  Whitespace & TODO highlighting
;;   10. LSP core (lsp-mode, lsp-ui, consult-lsp, navigation keys)
;;   10c. Debugging (dape / Debug Adapter Protocol)
;;   11. Languages: Python, Go, Rust, Clojure, Elixir, TS/JS, Terraform/HCL,
;;       GitHub Actions workflows
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
;;
;; The node bin directory is RESOLVED, not hardcoded: nvm installs each version
;; under its own path, so pinning one (it used to be v24.14.0) silently breaks
;; every node-installed binary the moment nvm upgrades and removes it.  That is
;; not hypothetical -- v24.14.0 was already gone while still listed here, which
;; hid `claude-agent-acp' (the ACP adapter, see section 10b) from the daemon.
(defun my/nvm-node-bin ()
  "Return the newest installed nvm node `bin' directory, or nil."
  (car (last (sort (seq-filter #'file-directory-p
                               (file-expand-wildcards
                                (expand-file-name "~/.nvm/versions/node/*/bin")))
                   #'string-version-lessp))))

(dolist (dir (delq nil (list "/Users/kanishk/elixir-ls/release"
                             (my/nvm-node-bin)
                             "/opt/homebrew/bin")))
  (add-to-list 'exec-path dir))

;; Keep the PATH env var in step with `exec-path'.  Subprocesses that re-exec
;; through a shell (cmake for vterm, `npx' under the ACP adapter) read PATH, not
;; `exec-path', so both have to carry the same directories.
(dolist (bin (delq nil (list "/opt/homebrew/bin" (my/nvm-node-bin))))
  (unless (member bin (split-string (or (getenv "PATH") "") path-separator))
    (setenv "PATH" (concat bin path-separator (getenv "PATH")))))

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
        ;; No native scroll bars; `my/scroll-bar-mode' draws a thin one in the fringe.
        (set-frame-parameter frame 'vertical-scroll-bars nil)
        ;; Left fringe 4 px (diagnostics), right fringe 8 px (scroll bar thumb).
        (fringe-mode '(4 . 3))))))

;; Run for every client frame the daemon creates...
(add-hook 'after-make-frame-functions #'my/frame-setup)
;; ...and immediately when Emacs is started non-daemon.
(unless (daemonp)
  (my/frame-setup))

;; Re-apply the color locks whenever a theme loads.
(advice-add 'load-theme :after #'my/lock-cursor-color)
(advice-add 'load-theme :after #'my/set-region-color)

;; Thin scroll bar, drawn in the right fringe and updated in real time.
;;
;; This replaces `yascroll', whose handler sits on `window-scroll-functions'
;; and so runs inside redisplay on every scroll event: 5.72 ms per event on
;; prometheus' tsdb/db_test.go (403k chars, 11.5k lines), and holding C-n
;; scrolls once per line once point reaches the window edge.
;;
;; Where that 5.72 ms actually goes, measured:
;;
;;     (move-to-window-line 0)                      2.63 ms
;;     (vertical-motion 26)                         1.01 ms
;;     two whole-buffer (count-lines ...)           0.43 ms
;;     everything else                             ~1.65 ms
;;
;; The obvious suspect is wrong.  Counting lines over the whole 403k-char
;; buffer is only 0.2 ms, because `count-lines' is C.  The real expense is
;; SCREEN-LINE LAYOUT: `move-to-window-line' and `vertical-motion' have to
;; work out where text actually lands, which depends on wrapping, display
;; properties, images and multi-line overlays.  No amount of line indexing
;; helps with that -- a line index answers "which logical line", and layout
;; is a different question.
;;
;; So the saving below comes from doing the layout work once, from
;; `window-start', rather than from `move-to-window-line' plus a second
;; walk.
;;
;; Sizing is by LOGICAL LINES.  Emacs' own native bar sizes by character
;; share instead, which redisplay can do in C for free but which is visibly
;; wrong on uneven files: in a 100-line buffer whose 4th line holds half
;; the characters, scrolling past that one line moved a character-sized
;; thumb from window line 1 to line 28 and shrank it from 42 segments to
;; 16.  Counting lines costs almost nothing, so there is no reason to
;; inherit that flaw.
;;
;; Cost is O(thumb position in screen lines), never O(buffer size): 1.3 ms
;; here against yascroll's 5.72 ms.
;;
;; Not drawn on tty frames: fringes are zero-width there, so there is
;; nothing to draw into.  yascroll had the same limitation, so no terminal
;; scroll bar has been lost.  Drawing one in the text area is possible but
;; needs its own handling of hscroll, truncation and line-number columns.

(defcustom my/scroll-bar-side 'right-fringe
  "Fringe to draw the scroll bar thumb in."
  :type '(choice (const right-fringe) (const left-fringe))
  :group 'convenience)

(defcustom my/scroll-bar-priority 20
  "Overlay priority for the thumb."
  :type 'integer
  :group 'convenience)

(defface my/scroll-bar-thumb
  '((t (:background "slateblue" :foreground "slateblue")))
  "Face for the scroll bar thumb."
  :group 'convenience)

(defvar-local my/scroll-bar--overlays nil
  "Thumb overlays currently drawn for this buffer, across all its windows.")

(defvar-local my/scroll-bar--total-lines nil
  "Cons of (MODIFIED-TICK . LINE-COUNT) for this buffer.")

(defvar-local my/scroll-bar--visible-lines nil
  "Logical lines the window shows.  See `my/scroll-bar--visible-lines'.")

(defun my/scroll-bar--total-lines ()
  "Line count of this buffer, recomputed only when it changes.
`count-lines' is C and scans the whole 403k-char db_test.go in 0.20 ms,
so this is cheap even uncached; the cache just makes it free."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql (car my/scroll-bar--total-lines) tick)
      (setq my/scroll-bar--total-lines
            (cons tick (count-lines (point-min) (point-max)))))
    (cdr my/scroll-bar--total-lines)))

(defun my/scroll-bar--clear ()
  "Delete every thumb overlay in this buffer."
  (mapc #'delete-overlay my/scroll-bar--overlays)
  (setq my/scroll-bar--overlays nil))

(defun my/scroll-bar--usable-side ()
  "Return the fringe to draw in, or nil if this window has none."
  (let ((fringes (window-fringes)))
    (pcase my/scroll-bar-side
      ('right-fringe (and (> (nth 1 fringes) 0) 'right-fringe))
      ('left-fringe  (and (> (nth 0 fringes) 0) 'left-fringe))
      (_ nil))))

(defun my/scroll-bar--make-overlay (side win)
  "Put one thumb segment on SIDE of WIN at point."
  (let* ((pos (point))
         ;; An overlay exactly at end of line puts the bitmap on this
         ;; visual line; anywhere else it needs the following character.
         (pos (if (= (line-end-position) pos) pos (1+ pos)))
         (ov (make-overlay pos pos)))
    (overlay-put ov 'after-string
                 (propertize "." 'display
                             `(,side filled-rectangle my/scroll-bar-thumb)))
    (overlay-put ov 'window win)
    (overlay-put ov 'priority my/scroll-bar-priority)
    ov))

(defun my/scroll-bar--visible-lines (win start settled)
  "Logical lines WIN currently shows, cached.

`window-end' is STALE inside `window-scroll-functions' -- it still
describes the window redisplay last drew, not the one about to be drawn.
Measured on a 53-line window jumping 50 lines: window-end reported line
2055 while the truth after redisplay was 2105, so a fresh computation
there yields 4 visible lines instead of 54.  That is what collapsed the
thumb to a sliver on C-v, M-v and fast trackpad scrolling.

How many lines a window shows is a property of the window, not of where
it is scrolled to, so it is cached and only recomputed when SETTLED --
that is, from the post-redisplay pass or a configuration change, never
from the scroll hook.  `window-body-height' seeds it before the first
settle; that is exact unless lines wrap."
  (cond
   ((and settled (window-end win))
    (setq my/scroll-bar--visible-lines
          (max 1 (count-lines start (window-end win)))))
   (my/scroll-bar--visible-lines)
   (t (setq my/scroll-bar--visible-lines (max 1 (window-body-height win))))))

(defun my/scroll-bar--draw (win &optional start settled)
  "Draw the thumb for WIN.  Assumes WIN is selected and already cleared.
START, when given, is the window start `window-scroll-functions' is about
to install, which `window-start' does not yet report.  SETTLED means
redisplay has finished, so `window-end' can be trusted."
  (let ((side (my/scroll-bar--usable-side)))
    (when side
      (let* ((h     (max 1 (window-body-height win)))
             (start (or start (window-start win)))
             (total (my/scroll-bar--total-lines)))
        (when (> total 0)
          (let* (;; Both in LOGICAL LINES, not characters.  Sizing by
                 ;; character share, the way Emacs' own native scroll bar
                 ;; does, is visibly wrong on uneven files: in a 100-line
                 ;; buffer whose 4th line holds half the characters,
                 ;; scrolling that single line moved the thumb from window
                 ;; line 1 to line 28 and shrank it from 42 segments to 16.
                 ;; Lines cost almost nothing to count here -- 0.075 ms for
                 ;; `line-number-at-pos' plus a cached total -- because the
                 ;; expensive part of a scroll bar was never line counting.
                 (visible (my/scroll-bar--visible-lines win start settled))
                 ;; Position, unlike size, is recomputed on every scroll and
                 ;; is exact: START is the value the hook is installing and
                 ;; `line-number-at-pos' reads text, not display state.
                 (start-line (1- (line-number-at-pos start))))
            ;; Whole buffer already on screen: no thumb, like a real one.
            (when (< visible total)
              (let* ((size (max 1 (min h (round (* h (/ (float visible) total))))))
                     (top  (max 0 (min (- h size)
                                       (floor (* h (/ (float start-line) total)))))))
                ;; The thumb sits on a screen line, and reaching screen line
                ;; TOP means asking Emacs to lay the text out: ~0.05 ms per
                ;; line, so up to ~2.5 ms at the bottom of a tall window.
                ;;
                ;; Interpolating a buffer position instead was tried and is
                ;; wrong.  Character fraction is a fine proxy for where the
                ;; thumb belongs across a whole buffer, but not for which
                ;; screen line it lands on inside one window, where a
                ;; handful of long lines skew it badly: scrolled to 25% the
                ;; thumb appeared on line 17 of 53 instead of 13, and at
                ;; 100% on line 0.  Correctness wins over the millisecond.
                (save-excursion
                  (if (and settled (/= 0 (window-vscroll win t)))
                      ;; A pixel scroll has left the window offset by a
                      ;; partial line.  `vertical-motion' from `window-start'
                      ;; then misses the intended window line by up to 4
                      ;; (measured: deltas of -4, -3, +1, +2 against
                      ;; `move-to-window-line').  `move-to-window-line' is
                      ;; authoritative and vscroll-aware; it costs ~3 ms, but
                      ;; this branch is only reached once the flick has
                      ;; stopped, never during it.
                      (move-to-window-line top)
                    (goto-char start)
                    (vertical-motion top win))
                  (cl-loop repeat size
                           do (push (my/scroll-bar--make-overlay side win)
                                    my/scroll-bar--overlays)
                           until (zerop (vertical-motion 1 win))))))))))))

(defun my/scroll-bar--refresh (&optional scrolled-window scrolled-start settled)
  "Redraw the thumb in every window showing this buffer.
SCROLLED-WINDOW and SCROLLED-START come from `window-scroll-functions',
which reports the start it is about to install before `window-start'
returns it.  SETTLED means redisplay has finished and display state can
be trusted.

The body runs with redisplay inhibited and the window hooks unbound.
Creating overlays is a display change, and this is itself called from
inside redisplay, so without that guard it can re-enter."
  (when (and (bound-and-true-p my/scroll-bar-mode)
             ;; Mid pixel-scroll the window sits at a partial-line offset and
             ;; the cheap placement is wrong by several lines, which is the
             ;; jitter you see dragging on a trackpad.  Leave the existing
             ;; thumb where it is rather than drawing it somewhere wrong; the
             ;; settle pass repositions it accurately the moment the flick
             ;; stops.  Pixel scrolls that land on a line boundary have
             ;; vscroll 0 and take the normal path, so the thumb still tracks
             ;; the scroll rather than freezing.
             (or settled
                 (zerop (window-vscroll (or scrolled-window (selected-window)) t))))
    ;; Never let a scroll bar break redisplay.
    (with-demoted-errors "scroll bar: %S"
      (let ((inhibit-redisplay t)
            window-configuration-change-hook
            window-size-change-functions
            window-state-change-hook)
        (my/scroll-bar--clear)
        (unless (minibufferp)
          (dolist (win (get-buffer-window-list (current-buffer) nil t))
            (with-selected-window win
              (my/scroll-bar--draw
               win (and (eq win scrolled-window) scrolled-start)
               settled)))
          (setq my/scroll-bar--last-view
                (my/scroll-bar--view-key (selected-window))))))))

;; --- Settling pass ---
;;
;; Two things cannot be known from inside `window-scroll-functions':
;; `window-end' is still the previous frame's, and pixel scrolling moves
;; sub-line via `window-vscroll' without changing `window-start' at all, so
;; the hook does not even fire.  A zero-delay idle timer runs after
;; redisplay has completed, when both are accurate, and corrects whatever
;; the scroll-time estimate got wrong.  It is debounced to one pending
;; timer, so a fast trackpad flick settles once rather than per event.
(defvar my/scroll-bar--settle-timer nil)

(defun my/scroll-bar--settle ()
  "Recompute with trustworthy display state, once redisplay is done."
  (setq my/scroll-bar--settle-timer nil)
  (when (bound-and-true-p my/scroll-bar-mode)
    (my/scroll-bar--refresh nil nil 'settled)))

(defun my/scroll-bar--schedule-settle ()
  (unless my/scroll-bar--settle-timer
    (setq my/scroll-bar--settle-timer
          (run-with-idle-timer 0 nil #'my/scroll-bar--settle))))

(defvar-local my/scroll-bar--last-view nil
  "Cons of (WINDOW-START . VSCROLL) at the last draw.")

(defun my/scroll-bar--view-key (win)
  (cons (window-start win) (window-vscroll win t)))

(defun my/scroll-bar--on-command ()
  "Catch the scrolling `window-scroll-functions' never reports.

That hook fires only when `window-start' changes.  `pixel-scroll-precision'
-- which is what `wheel-up' and `wheel-down' are bound to here -- moves
within a line by changing `window-vscroll' and leaving `window-start'
alone.  Measured: one 40px trackpad scroll left vscroll at 8, fired the
scroll hook zero times, and scheduled no settle at all.  That is the thumb
sitting still while the text moves.

Comparing the view key first makes this free after ordinary commands, so
it does not put a redraw behind every keystroke."
  (unless (equal (my/scroll-bar--view-key (selected-window))
                 my/scroll-bar--last-view)
    (my/scroll-bar--schedule-settle)))

(defun my/scroll-bar--on-scroll (win start)
  "Entry point for `window-scroll-functions'."
  (my/scroll-bar--refresh win start)
  (my/scroll-bar--schedule-settle))

(defun my/scroll-bar--on-change (&rest _)
  "Entry point for `after-change-functions' and friends.
Takes and discards any arguments: `after-change-functions' passes three,
`window-configuration-change-hook' passes none."
  (my/scroll-bar--refresh)
  (my/scroll-bar--schedule-settle))

(define-minor-mode my/scroll-bar-mode
  "Thin fringe scroll bar whose cost does not depend on buffer size."
  :lighter nil
  (if my/scroll-bar-mode
      (progn
        (add-hook 'window-scroll-functions #'my/scroll-bar--on-scroll nil t)
        (add-hook 'window-configuration-change-hook #'my/scroll-bar--on-change nil t)
        ;; Editing moves `point-max', so the thumb has to resize.  This is
        ;; O(window height), not O(buffer), so it is affordable per
        ;; keystroke -- which is exactly what yascroll could not say, since
        ;; it counted the whole buffer on every change too.
        (add-hook 'after-change-functions #'my/scroll-bar--on-change nil t)
        (add-hook 'post-command-hook #'my/scroll-bar--on-command nil t)
        (my/scroll-bar--refresh nil nil 'settled))
    (remove-hook 'post-command-hook #'my/scroll-bar--on-command t)
    (remove-hook 'window-scroll-functions #'my/scroll-bar--on-scroll t)
    (remove-hook 'window-configuration-change-hook #'my/scroll-bar--on-change t)
    (remove-hook 'after-change-functions #'my/scroll-bar--on-change t)
    (my/scroll-bar--clear)))

(defun my/scroll-bar--turn-on ()
  "Enable the scroll bar where it makes sense."
  (unless (or (minibufferp)
              (derived-mode-p 'image-mode)
              (string-prefix-p " " (buffer-name)))
    (my/scroll-bar-mode 1)))

(define-globalized-minor-mode global-my/scroll-bar-mode
  my/scroll-bar-mode my/scroll-bar--turn-on)

(defun my/scroll-bar-toggle (&optional arg)
  "Toggle the fringe scroll bar everywhere.
With a positive prefix ARG turn it on, with a negative one turn it off.
This drives the globalized mode, so it also removes the per-buffer hooks
in buffers that already have it, not just the visible thumb."
  (interactive "P")
  (global-my/scroll-bar-mode
   (cond ((null arg) (if global-my/scroll-bar-mode -1 1))
         ((> (prefix-numeric-value arg) 0) 1)
         (t -1)))
  (message "Fringe scroll bar %s"
           (if global-my/scroll-bar-mode "on" "off")))

(scroll-bar-mode -1)                     ;; kill the native NS scroller
(global-my/scroll-bar-mode 1)

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

;; Snippet engine.  Installed for one specific reason: lsp-mode only
;; advertises snippet support to a server when yasnippet is present
;; (`lsp-enable-snippet', section 10).  Without it gopls silently stops
;; sending parameter placeholders and completing a call gives you a bare
;; identifier instead of a filled-in argument list.
;;
;; Deliberately no `yasnippet-snippets' library: with an empty snippet
;; table nothing competes for TAB, and everything that matters here comes
;; from the language server rather than from hand-written templates.
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

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

;; Prelude's language modules all call `prelude-lsp-enable', and
;; `prelude-lsp-client' defaults to `eglot' (see core/prelude-custom.el).
;; So every Go / Python / JS buffer was starting eglot *in addition to*
;; the lsp-mode client hooked below: two language servers on the same
;; project, two diagnostics backends (that is the `Flymake[..]' next to
;; `FlyC:..' in the modeline), two completion-at-point stacks and two
;; format-on-save paths.  nil makes `prelude-lsp-enable' a no-op, so this
;; file owns LSP outright.
(setq prelude-lsp-client nil)

(use-package lsp-mode
  :ensure t
  :init
  ;; Corfu is the one and only completion UI.
  ;;
  ;; `:none' is the ONLY value that leaves the completion UI alone.  In
  ;; lsp-mode's vocabulary `:capf' is short for *company*-capf -- its own
  ;; defcustom reads `(const :tag "Use company-capf" :capf)' -- and
  ;; `lsp-completion.el' acts on anything other than `:none' by running
  ;;
  ;;     (company-mode 1)
  ;;     (setq-local company-backends (cl-adjoin 'company-capf ...))
  ;;
  ;; in every managed buffer.  That is how company-mode came to be live in
  ;; every Go buffer here despite `global-company-mode' being nil, no hook
  ;; enabling it, and `prelude-company' being commented out: lsp-mode was
  ;; switching it on, buffer by buffer, alongside Corfu.
  ;;
  ;; Nothing is lost by saying `:none'.  `lsp-completion-at-point' is added
  ;; to `completion-at-point-functions' before that `cond' is reached, so it
  ;; stays first in the list and Corfu reads it exactly as before.  `:none'
  ;; also emits no warning whether or not company is installed, which is
  ;; what the previous comment here was worried about.
  ;;
  ;; The `company' package is uninstalled.  Keep `prelude-company' commented
  ;; out in personal/prelude-modules.el -- it does
  ;; (prelude-require-packages '(company)) and would quietly undo this.
  (setq lsp-completion-provider :none)
  ;; Snippet completion.  gopls only sends parameter placeholders
  ;; (`lsp-go-use-placeholders' and `lsp-go-complete-function-calls', both
  ;; t by default) when the client advertises snippet support, and that
  ;; needs yasnippet -- installed in section 7.  This is the difference
  ;; between completing `fmt.Println' and completing `fmt.Println(a ...any)'
  ;; with point already in the argument slot.
  (setq lsp-enable-snippet t)
  ;; Prefix for lsp-mode's whole command map (see the GoLand key table
  ;; further down).  The default is `s-l', i.e. Cmd-l, which a terminal
  ;; frame cannot deliver at all.  `C-c L' works in GUI and tty and steps
  ;; on nothing: `C-c l' is org-store-link, `C-c C-l' is cider-load-file.
  (setq lsp-keymap-prefix "C-c L")
  ;; Inlay hints: parameter names and inferred types drawn inline, the way
  ;; GoLand does.  This variable is the supported switch -- lsp-mode reads
  ;; it in `lsp-configure-buffer' and turns `lsp-inlay-hints-mode' on
  ;; itself, but only once the workspace is up and the server has actually
  ;; advertised textDocument/inlayHint.
  ;;
  ;; Do NOT put `lsp-inlay-hints-mode' in a use-package `:hook' instead.
  ;; That function carries no autoload cookie, so at the time this file is
  ;; read it is not yet fbound, and use-package responds by generating
  ;; `(autoload (quote lsp-inlay-hints-mode) "go-ts-mode")' -- an autoload
  ;; pointing at a file that does not define it.  Opening a Go buffer then
  ;; fails with "Autoloading file .../go-ts-mode.elc failed to define
  ;; function lsp-inlay-hints-mode".  A mode hook is also simply too early:
  ;; it runs before the server is connected.
  (setq lsp-inlay-hint-enable t)
  ;; ...but do not recompute them from inside redisplay.  lsp-mode puts
  ;; `lsp--update-inlay-hints-scroll-function' on `window-scroll-functions'
  ;; when this is non-nil, and that handler calls (window-end window t) --
  ;; forcing a display simulation -- then fires a request, on every scroll
  ;; event.  Measured at 4.56 ms per scroll on a 403k-char buffer.
  ;; Hints still refresh through `lsp-on-idle-hook', which
  ;; `lsp-inlay-hints-mode' also registers, so nothing is lost except
  ;; mid-scroll updates.
  (setq lsp-update-inlay-hints-on-scroll nil)
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
  ;; Always open the peek list, even when the server returns exactly one
  ;; result.  Without this lsp-ui silently jumps instead, so a lone
  ;; implementation behaves differently from two -- and you lose the one
  ;; view that shows you it IS the only one.  Applies to every peek
  ;; command: M-o (implementations), M-? (references) and C-c L G g
  ;; (definitions).
  (lsp-ui-peek-always-show t)
  :hook
  (lsp-mode . lsp-ui-mode))

;; Workspace symbol search, file structure and a project-wide problems
;; list, all through the vertico/consult UI already in use here.  These
;; are GoLand's "Search Everywhere for symbols", "File Structure" and
;; "Problems" views.  lsp-mode also routes `xref-find-apropos' at
;; `C-c L g a' to workspace symbols; consult-lsp gives live narrowing.
(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult))

;; lsp-treemacs supplies the two hierarchy views lsp-mode has entries for
;; but cannot draw on its own.  With this installed, `C-c L g h' becomes
;; GoLand's call hierarchy (Ctrl+Alt+H), `C-c L g y' its type hierarchy
;; (Ctrl+H) and `C-c L g e' its problems list.
;;
;; Autoloaded only.  It drags in treemacs, so it is left to load on first
;; use rather than at lsp-mode load time; `lsp-treemacs-sync-mode' is
;; deliberately off, since file navigation here goes through projectile
;; and dired, not a treemacs sidebar.
(use-package lsp-treemacs
  :ensure t
  :commands (lsp-treemacs-call-hierarchy
             lsp-treemacs-type-hierarchy
             lsp-treemacs-errors-list
             lsp-treemacs-symbols
             lsp-treemacs-references
             lsp-treemacs-implementations))

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

;; --- Inlay hint colour ------------------------------------------------
;;
;; lsp-mode ships `lsp-inlay-hint-face' inheriting `font-lock-comment-face'.
;; Under doom-dark+ that resolves to green (#579C4C), so hints read as
;; comments rather than as editor chrome.  GoLand and VS Code both draw
;; them in a neutral dim grey; #969696 is literally VS Code Dark+'s
;; `editorInlayHint.foreground', which pairs with the #1e1e1e background
;; this theme already uses.
;;
;; `lsp-inlay-hint-type-face' and `lsp-inlay-hint-parameter-face' both
;; inherit the base face, so setting the base covers both.  The explicit
;; `unspecified' inherit is what detaches it from the comment face.
;;
;; Re-applied after `load-theme' because loading a theme re-evaluates face
;; specs and would otherwise put the comment colour back.
(defun my/lsp-style-inlay-hints (&rest _)
  "Draw LSP inlay hints in a dim grey, the way GoLand and VS Code do."
  (when (facep 'lsp-inlay-hint-face)
    (set-face-attribute 'lsp-inlay-hint-face nil
                        :inherit 'unspecified
                        :foreground "#969696"
                        :background 'unspecified
                        :slant 'normal
                        :weight 'normal)))

(with-eval-after-load 'lsp-mode
  (my/lsp-style-inlay-hints))
(advice-add 'load-theme :after #'my/lsp-style-inlay-hints)

;; --- Inlay hint range: whole buffer for normal files ------------------
;;
;; lsp-mode asks only for the visible window, so hints for anything you
;; scroll to arrive `lsp-idle-delay' later.  The obvious fix -- request a
;; band of a few hundred lines around the window -- turns out not to be a
;; thing gopls offers.  Measured against gopls v0.23 on prometheus:
;;
;;     buffer        range asked          hints back   latency
;;     db_test.go    20 lines @ L5000         63         14 ms
;;     db_test.go    50 lines @ L5000         63         13 ms
;;     db_test.go   100 lines @ L5000       9549        154 ms
;;     db_test.go   400 lines @ L5000       9549        372 ms
;;     db_test.go   whole file (11.5k)      9549        442 ms
;;     db_test.go    40 lines @ L1          9549        151 ms
;;     main.go       40 lines @ L1          1076         33 ms
;;
;; Note the last two rows: a 40-line range answers with the whole file, so
;; this is not a simple size cut-off and I do not have a model for what
;; gopls is really keying on.  What is reliable is the practical shape --
;; asking for a mid-sized band gets you the whole file anyway, so the only
;; two honest choices are visible-window or whole-buffer.
;;
;; Whole-buffer is affordable now that the scroll hooks are out of the way:
;; a C-n step costs 0.265 ms with all 9549 hints present, against 0.096 ms
;; with only the visible 248 overlays, and 2.819 ms back when this felt
;; laggy.  What it buys is that scrolling never waits for hints again,
;; because they are already everywhere.
;;
;; What it costs is one request per EDIT rather than per scroll, and that
;; request grows with file size.  Hence the ceiling: above it, fall back to
;; lsp-mode's visible-window behaviour rather than tie gopls up for half a
;; second every time you pause typing in a very large file.

;; The freeze this fixes, measured on db_test.go with a 50 ms heartbeat
;; timer detecting UI stalls.  Isolating each `lsp-on-idle-hook' entry over
;; four scroll-then-pause cycles:
;;
;;     nothing              max   59 ms   total frozen     0 ms
;;     inlay-hints-only     max 2492 ms   total frozen  7335 ms
;;     lens-only            max  125 ms   total frozen     0 ms
;;     doc-highlight-only   max   59 ms   total frozen     0 ms
;;     doc-links-only       max   67 ms   total frozen     0 ms
;;     breadcrumb-only      max  102 ms   total frozen     0 ms
;;     code-actions-only    max   71 ms   total frozen     0 ms
;;
;; The cost is not the request (151 ms) and not raw overlay creation (7 ms
;; for 9549 overlays).  It is `lsp--position-to-point', which lsp-mode's
;; renderer calls once per hint and which is O(line):
;;
;;     (goto-char (point-min)) (forward-line line)
;;
;; 9549 hints averaging line ~5800 is roughly 55 million line steps per
;; response.  So the fix is not to render fewer overlays, it is to avoid
;; converting positions for hints nobody can see.  Every InlayHint carries
;; its line as a plain integer, so filtering on that costs nothing and only
;; the visible handful ever pays the conversion.

(defcustom my/lsp-inlay-hint-render-margin-lines 200
  "Lines above and below the window for which inlay hints are drawn.
The whole buffer is fetched once and cached; this only controls how much
of that cache is turned into overlays at a time."
  :type 'integer
  :group 'lsp-mode)

(defvar-local my/lsp-inlay--tick nil
  "`buffer-chars-modified-tick' at the last whole-buffer hint request.")

(defvar-local my/lsp-inlay--retries 0
  "Consecutive whole-buffer requests that produced no hints.")

(defvar-local my/lsp-inlay--cache nil
  "Raw InlayHint objects from the last whole-buffer response.")

(defvar-local my/lsp-inlay--rendered nil
  "Cons of (TOP-LINE . BOTTOM-LINE), zero-based, currently drawn.")

(defun my/lsp-inlay--draw (hint)
  "Create the overlay for HINT.
A faithful copy of the body of `lsp-update-inlay-hints', which offers no
way to render a subset.  Being a copy, it can drift on an lsp-mode
upgrade; if hints ever look wrong, diff it against that function first.

Deliberately NOT written with lsp-mode's `(&InlayHint :label ...)'
destructuring, which is what that function uses.  That pattern is resolved
by a `dash-expand:&InlayHint' macro which `lsp-protocol' only defines once
it loads -- and this file is read at startup, long before lsp-mode is
pulled in.  With the macro absent, `-let*' silently falls back to ordinary
list destructuring and every hint dies at runtime with

    wrong-type-argument listp #s(hash-table ... \"line\" 645 ...)

Plain accessor FUNCTIONS resolve when they are called instead of when this
file is read, so they are immune to the load order."
  (let* ((label         (lsp:inlay-hint-label hint))
         (position      (lsp:inlay-hint-position hint))
         (kind          (or (lsp:inlay-hint-kind? hint)
                            lsp/inlay-hint-kind-type-hint))
         (padding-left? (lsp:inlay-hint-padding-left? hint))
         (padding-right? (lsp:inlay-hint-padding-right? hint))
         (tooltip?      (lsp:inlay-hint-tooltip? hint))
         (label-str     (lsp--label-from-inlay-hints-response label kind))
         (pos           (lsp--position-to-point position)))
    (when label-str
      (let ((overlay (make-overlay pos pos nil 'front-advance 'end-advance)))
        (overlay-put overlay 'lsp-inlay-hint t)
        (overlay-put overlay 'lsp-inlay-hint-data hint)
        (overlay-put overlay 'before-string
                     (propertize
                      (format "%s%s%s"
                              (if padding-left? " " "")
                              (let ((s label-str))
                                (when (and tooltip? (stringp label))
                                  (setq s (propertize
                                           s 'help-echo
                                           (lsp--inlay-hint-tooltip-text tooltip?))))
                                s)
                              (if padding-right? " " ""))
                      'keymap lsp--inlay-hint-mouse-map))))))

(defun my/lsp-inlay--render (&optional force)
  "Draw cached hints for the window plus `my/lsp-inlay-hint-render-margin-lines'.
Filtering happens on the line number carried in each hint, an integer
already in the response, so out-of-band hints never reach the expensive
`lsp--position-to-point'."
  (when my/lsp-inlay--cache
    (let* ((m    my/lsp-inlay-hint-render-margin-lines)
           ;; LSP lines are zero-based, `line-number-at-pos' is one-based.
           (wtop (1- (line-number-at-pos (window-start))))
           (wbot (1- (line-number-at-pos (or (window-end) (point-max)))))
           (top  (max 0 (- wtop m)))
           (bot  (+ wbot m)))
      ;; Hysteresis: only redraw once the window leaves the drawn band, so
      ;; ordinary scrolling inside the margin costs nothing at all.
      (when (or force
                (null my/lsp-inlay--rendered)
                (< wtop (car my/lsp-inlay--rendered))
                (> wbot (cdr my/lsp-inlay--rendered)))
        (lsp--remove-overlays 'lsp-inlay-hint)
        (setq my/lsp-inlay--rendered (cons top bot))
        (dolist (hint my/lsp-inlay--cache)
          (let ((ln (lsp:position-line (lsp:inlay-hint-position hint))))
            (when (and (>= ln top) (<= ln bot))
              (my/lsp-inlay--draw hint))))))))

(defun my/lsp-inlay--fetch ()
  "Request every hint in the buffer and cache the response."
  (let ((buf (current-buffer)))
    (lsp-request-async
     "textDocument/inlayHint"
     (lsp-make-inlay-hints-params
      :text-document (lsp--text-document-identifier)
      :range (lsp-make-range :start (lsp-point-to-position (point-min))
                             :end   (lsp-point-to-position (point-max))))
     (lambda (res)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq my/lsp-inlay--cache res
                 my/lsp-inlay--rendered nil)
           (my/lsp-inlay--render t))))
     ;; `tick' and the cancel token match what lsp-mode uses: drop the
     ;; response if the buffer changed under it, and supersede any request
     ;; still in flight.
     :mode 'tick
     :cancel-token :inlay-hints)))

(defun my/lsp-update-inlay-hints ()
  "Override for `lsp--update-inlay-hints'.
Fetch the whole buffer once per edit, then serve scrolling from the cache.
No size threshold: the visible-window path lsp-mode uses is strictly worse
here, because gopls answers with every hint in the file whatever range you
ask for, so that path paid the identical cost on every single pause."
  (let ((tick (buffer-chars-modified-tick)))
    (cond
     ;; First visit, or the buffer changed: refetch.
     ((not (eql tick my/lsp-inlay--tick))
      (setq my/lsp-inlay--tick tick
            my/lsp-inlay--retries 0)
      (my/lsp-inlay--fetch))
     ;; Have data: redraw only if the window left the drawn band.
     (my/lsp-inlay--cache
      (my/lsp-inlay--render))
     ;; Requested, nothing arrived.  gopls silently ignored these for about
     ;; a minute while re-indexing after a restart, which would otherwise
     ;; leave the buffer bare until the next edit.
     ((< my/lsp-inlay--retries 10)
      (setq my/lsp-inlay--retries (1+ my/lsp-inlay--retries))
      (my/lsp-inlay--fetch)))))

(defun my/lsp-inlay--reset-tick ()
  "Force the next idle tick to refetch hints in this buffer.
Toggling `lsp-inlay-hints-mode' deletes every hint overlay; without this
the tick would still match and they would never come back."
  (setq my/lsp-inlay--tick nil
        my/lsp-inlay--retries 0
        my/lsp-inlay--cache nil
        my/lsp-inlay--rendered nil))

(defun my/lsp-inlay--reset-all-ticks (&rest _)
  "Re-request hints everywhere after a language server (re)connects.
A gopls restart drops whatever it knew, and the per-buffer tick would
otherwise still match and suppress the request."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (bound-and-true-p lsp-inlay-hints-mode)
        (my/lsp-inlay--reset-tick)))))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--update-inlay-hints :override #'my/lsp-update-inlay-hints)
  (add-hook 'lsp-inlay-hints-mode-hook #'my/lsp-inlay--reset-tick)
  (add-hook 'lsp-after-initialize-hook #'my/lsp-inlay--reset-all-ticks))

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

;; --- GoLand navigation parity ---
;;
;; `lsp-keymap-prefix' is `C-c L' (set in the lsp-mode block above), which
;; exposes lsp-mode's entire command map with which-key annotating every
;; leaf.  Everything GoLand does over LSP is already in there; the map
;; below is the translation, plus three short keys for the operations that
;; get used every few minutes.
;;
;;   GoLand                              here
;;   ---------------------------------   -----------------------------------
;;   Ctrl+B         declaration          M-.     / C-c L g g
;;   Ctrl+Alt+B     implementations      M-o     / C-c L G i   (peek list)
;;                  ... jump straight    M-O     / C-c L g i
;;   Ctrl+Shift+B   type declaration     C-c L g t
;;   Alt+F7         find usages          M-?     / C-c L g r
;;   Ctrl+Alt+Shift+N  symbol in project C-c L g a   (or M-x consult-lsp-symbols)
;;   Ctrl+Alt+H     call hierarchy       C-c L g h
;;   Ctrl+H         type hierarchy       C-c L g y
;;   Ctrl+F12       file structure       M-x consult-lsp-file-symbols
;;   Alt+Enter      quick fix / refactor M-RET   / C-c L a a
;;   Shift+F6       rename               C-c L r r
;;   Ctrl+Q         quick documentation  C-c L h g
;;   Ctrl+P         parameter info       C-c L h s
;;   Problems view                       C-c L g e   (or consult-lsp-diagnostics)
;;   Code Vision: N usages / N impls     C-c L g u   (on demand, see below)
;;                ... as a live lens     C-c L T u   (toggle, off by default)
;;   Optimize imports                    C-c L r o
;;   Reformat                            C-c L = =
;;   Toggle sideline/doc/hints/lenses    C-c L T ...
;;
;; The one worth internalising for Go: gopls implements
;; textDocument/implementation in BOTH directions.  With point on an
;; interface, or on a method inside an interface, `M-o' lists every
;; concrete type (or method) that satisfies it.  With point on a concrete
;; method, the same key lists the interfaces that method satisfies -- that
;; is GoLand's "Go to Super Method" (Ctrl+U) and its interface gutter
;; arrow, from one keystroke.  It works across the whole module, including
;; dependencies, because gopls indexes them.
(with-eval-after-load 'lsp-mode
  ;; Type hierarchy has no slot in lsp-mode's stock command map; give it
  ;; one next to the call hierarchy under `g'.
  (when (boundp 'lsp-command-map)
    (define-key lsp-command-map (kbd "g y") #'lsp-treemacs-type-hierarchy))

  ;; Short keys, chosen to survive a terminal frame (no super, and no C-i
  ;; which a tty cannot tell apart from TAB).  `M-i' is deliberately left
  ;; free: it is the "give context to the model" key in claude-code-ide
  ;; and the ACP agent-shell integration.
  ;;
  ;; `M-o' is `crux-smart-open-line' in prelude-mode-map.  Both are
  ;; minor-mode maps, so which one wins comes down to their order in
  ;; `minor-mode-map-alist' -- the same precedence problem the vterm
  ;; section below works around.  Rather than rely on lsp-mode having
  ;; loaded after prelude, hoist its entry to the front explicitly.  That
  ;; also protects M-. / M-? / M-, / M-RET in every LSP buffer.  crux keeps
  ;; M-o everywhere LSP is not running.
  (define-key lsp-mode-map (kbd "M-RET") #'lsp-execute-code-action)
  ;; M-o always opens the peek list, even for a single implementation --
  ;; that is `lsp-ui-peek-always-show' in the lsp-ui block above.  M-O is
  ;; the direct jump, for when you already know there is only one.
  (define-key lsp-mode-map (kbd "M-O")   #'lsp-find-implementation)
  (with-eval-after-load 'lsp-ui-peek
    (define-key lsp-mode-map (kbd "M-o") #'lsp-ui-peek-find-implementation))

  (let ((entry (assq 'lsp-mode minor-mode-map-alist)))
    (when entry
      (setq minor-mode-map-alist
            (cons entry (delq entry minor-mode-map-alist))))))

;; --- Usage and implementation counts (GoLand's Code Vision) -----------
;;
;; gopls does not provide these.  Verified against the installed binary
;; with `gopls api-json': it publishes exactly eight code lenses --
;; generate, regenerate_cgo, test, run_govulncheck, tidy,
;; upgrade_dependency, vendor, vulncheck -- and not one is a usage count.
;;
;; GoLand's "3 usages / 2 implementations" above a declaration is IntelliJ
;; Code Vision, answered instantly from a persistent whole-project index.
;; gopls has no such index; it resolves references by searching the
;; workspace per query, which is why upstream never turned counts into
;; lenses.  So they have to be asked for from this side: one
;; textDocument/references plus one textDocument/implementation per
;; declaration, each a workspace-wide search on the server.
;;
;; Entry points:
;;
;;   M-x my/lsp-usage-lens-global-mode   one switch for every buffer
;;   C-c L T u                           the same toggle
;;   C-c L g u                           one-shot count for the symbol at point
;;
;; "Project-wide" can only ever mean "every buffer I open": an overlay
;; needs a buffer, and files that are not visited have nothing to draw on.
;; The global mode is therefore a globalized minor mode -- flip it once and
;; every LSP buffer, present and future, carries lenses until you flip it
;; back.
;;
;; How much each buffer shows is `my/lsp-usage-lens-scope', because the
;; cost is per declaration, not per project:
;;
;;   at-point  only the declaration point is inside.  Two requests per
;;             declaration you visit.  Effectively free.
;;   window    every declaration currently on screen.  Two requests each,
;;             capped by `my/lsp-usage-lens-max'.  This is what GoLand
;;             looks like, and it is the one that can make gopls feel slow:
;;             completion and diagnostics queue behind these searches.

(defcustom my/lsp-usage-lens-scope 'at-point
  "How much of the buffer `my/lsp-usage-lens-mode' annotates.
`at-point' covers only the declaration containing point.  `window'
covers every declaration currently visible, which looks like GoLand but
costs two language-server searches per declaration."
  :type '(choice (const :tag "Declaration at point" at-point)
                 (const :tag "Everything on screen" window))
  :group 'lsp-mode)

(defcustom my/lsp-usage-lens-max 25
  "Most declarations to annotate in one pass when scope is `window'.
A backstop against a screenful of small functions turning into a burst of
workspace searches.  Truncation is reported, never silent."
  :type 'integer
  :group 'lsp-mode)

(defcustom my/lsp-usage-lens-idle 0.6
  "Idle seconds before the usage lens refreshes."
  :type 'number
  :group 'lsp-mode)

(defface my/lsp-usage-lens-face
  '((t :inherit lsp-inlay-hint-face :height 0.9))
  "Face for the usage counts drawn at the end of a declaration line.
Inheriting from `lsp-inlay-hint-face' means this tracks the grey that
`my/lsp-style-inlay-hints' sets, including after a theme change, without
needing its own `load-theme' advice.

Keep `:height' at or below 1.0.  A screen row is as tall as its tallest
glyph, so a smaller face cannot grow the row -- measured at 16 px with
and without the annotation.  A taller face would grow it, and a row that
changes height as the lens arrives is the vertical jitter that drawing at
end of line exists to avoid.

Vertical placement is handled by `my/lsp-usage-lens--raise-factor', which
reads this face's real metrics, so changing the height here needs no
other edit."
  :group 'lsp-mode)

(defvar my/lsp-usage-lens--raise nil
  "Cached (KEY . FACTOR) for `my/lsp-usage-lens--raise-factor'.")

(defun my/lsp-usage-lens--raise-factor ()
  "Factor for `raise' that vertically centres the lens text in its row.
Every glyph on a screen line shares one baseline, placed at the row's
maximum ascent, so a smaller font is inset unevenly.  Menlo 14 measures
ascent 13 descent 3, and scaling the height shrinks only the ascent: at
0.8 the face is 10/3, which puts the whole 3 px of slack above the text
and none below it.  That is why an unraised lens reads as sitting on the
bottom of the row rather than in the middle of it.

Centring means lifting by half the difference of the two insets,

  raise_px = ((Ad - Dd) - (As - Ds)) / 2

and `raise' takes a multiple of the raised text's own height, hence the
final division.  At 0.9 that is 0.5 px, against 1.5 px at 0.8.

Cached because this runs once per lens drawn.  The key covers the frame
font size, the face height, and whether the frame is graphical at all --
`font-info' has nothing to report on a tty, and this daemon serves both."
  (let ((key (list (frame-char-height)
                   (face-attribute 'my/lsp-usage-lens-face :height nil t)
                   (display-graphic-p))))
    (unless (equal (car my/lsp-usage-lens--raise) key)
      (setq my/lsp-usage-lens--raise
            (cons key
                  (or (and (display-graphic-p)
                           (ignore-errors
                             (let* ((di (font-info (face-font 'default)))
                                    (si (font-info (face-font 'my/lsp-usage-lens-face)))
                                    (ad (aref di 8)) (dd (aref di 9))
                                    (as (aref si 8)) (ds (aref si 9)))
                               (unless (zerop (+ as ds))
                                 (/ (/ (- (- ad dd) (- as ds)) 2.0)
                                    (float (+ as ds)))))))
                      0))))
    (cdr my/lsp-usage-lens--raise)))

(defvar my/lsp-usage-lens--timer nil
  "Shared idle timer driving `my/lsp-usage-lens-mode' in every buffer.")

(defvar-local my/lsp-usage-lens--overlays nil
  "Alist of (DECLARATION-START . OVERLAY) for this buffer.")

(defvar-local my/lsp-usage-lens--wanted nil
  "Declaration start positions that should currently carry a lens.")

(defvar-local my/lsp-usage-lens--tick nil
  "Value of `buffer-chars-modified-tick' when the lenses were drawn.")

(defvar-local my/lsp-usage-lens--decl-cache nil
  "Cons of (MODIFIED-TICK . DECLARATION-NODES) for this buffer.")

(defun my/lsp--count-locations (res)
  "Count the locations in RES.
A references or implementation response is either a collection of
Locations or a single one, and how each is represented depends on
`lsp-use-plists' -- nil here, so objects arrive as hash tables.  Test the
SINGLE-object shapes first, then treat anything else sequence-like as a
collection.

Getting this wrong is not hypothetical.  An earlier version asked
\(consp (car res)) to decide whether RES was a list of objects.  A list of
hash tables fails that test, so every non-empty answer fell through to the
single-object branch and reported 1: `serverOnlyFlag' in prometheus, with
31 real references, displayed \"1 usage\".  Zero-reference symbols looked
correct, which is what let it pass unnoticed."
  (cond ((null res) 0)
        ;; One Location, as a hash table (lsp-use-plists nil) ...
        ((hash-table-p res) 1)
        ;; ... or as a plist (lsp-use-plists t).
        ((and (consp res) (keywordp (car res))) 1)
        ;; Otherwise a vector or list of Location objects.
        ((sequencep res) (length res))
        (t 1)))

(defun my/lsp-usage-counts ()
  "Echo the reference and implementation counts for the symbol at point.

Usages EXCLUDE the declaration itself, matching what GoLand counts.  This
is deliberately one less than the peek window on `M-?' reports:
`lsp-ui-peek-find-references' passes nil into the `exclude-declaration'
slot of `lsp--make-reference-params', so its header counts the declaration
line as a reference.  31 usages there means 32 references.

The implementation clause is omitted rather than reported as zero when the
server refuses the query -- gopls answers \"X is a function, not a method\"
for a plain function, and printing \"0 implementations\" for that would be
a claim, not an absence."
  (interactive)
  (unless (bound-and-true-p lsp-mode)
    (user-error "No LSP session in this buffer"))
  (let ((sym (or (thing-at-point 'symbol t) "symbol"))
        (refs :pending)
        (impls :pending))
    (cl-labels
        ((report ()
           (unless (or (eq refs :pending) (eq impls :pending))
             ;; nil impls means "not applicable"; 0 is a real answer.
             (message "%s: %d usage%s%s"
                      sym
                      refs (if (eql refs 1) "" "s")
                      (if impls
                          (format ", %d implementation%s"
                                  impls (if (eql impls 1) "" "s"))
                        "")))))
      (lsp-request-async
       "textDocument/references" (lsp--make-reference-params nil t)
       (lambda (res) (setq refs (my/lsp--count-locations res)) (report))
       :mode 'alive
       :error-handler (lambda (_) (setq refs 0) (report)))
      (lsp-request-async
       "textDocument/implementation" (lsp--text-document-position-params)
       (lambda (res) (setq impls (my/lsp--count-locations res)) (report))
       :mode 'alive
       :error-handler (lambda (_) (setq impls nil) (report))))))

(defun my/lsp-usage-lens--name-node (node)
  "Return the identifier node that names declaration NODE.
References resolve against the name, not the `func' or `type' keyword, so
the request has to be issued from the identifier's position."
  (or (treesit-node-child-by-field-name node "name")
      ;; A Go `type_declaration' wraps its name one level down, in a
      ;; `type_spec'.
      (let ((spec (treesit-search-subtree node "\\`type_spec\\'" nil nil 2)))
        (and spec (treesit-node-child-by-field-name spec "name")))))

(defun my/lsp-usage-lens--clear ()
  "Delete every lens overlay in this buffer."
  (dolist (cell my/lsp-usage-lens--overlays)
    (when (overlayp (cdr cell)) (delete-overlay (cdr cell))))
  (setq my/lsp-usage-lens--overlays nil
        my/lsp-usage-lens--wanted nil
        my/lsp-usage-lens--tick nil
        my/lsp-usage-lens--decl-cache nil))

(defun my/lsp-usage-lens--all-declarations ()
  "Every top-level declaration node in this buffer, cached until it changes.
Walking all top-level children and matching `treesit-defun-type-regexp'
measured 0.41 ms on a 403k-char file, and it ran on every idle tick even
though nothing had moved.  The parse tree only changes when the buffer
does, so the scan is keyed on `buffer-chars-modified-tick'; what remains
per tick is a numeric range filter over the cached list."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql (car my/lsp-usage-lens--decl-cache) tick)
      (let ((rx (or treesit-defun-type-regexp ""))
            (hits '()))
        (dolist (child (treesit-node-children (treesit-buffer-root-node) t))
          (let ((type (treesit-node-type child)))
            (when (and (stringp type) (string-match-p rx type))
              (push child hits))))
        (setq my/lsp-usage-lens--decl-cache (cons tick (nreverse hits)))))
    (cdr my/lsp-usage-lens--decl-cache)))

(defun my/lsp-usage-lens--wants-impls-p (node)
  "Non-nil when asking gopls for implementations of NODE is meaningful.
gopls rejects textDocument/implementation on a plain function -- it
answers \"X is a function, not a method\" -- so a `function_declaration'
carries the usage count alone.  Methods and named types are the two kinds
where interface satisfaction is an actual question.  Skipping the request
also halves the traffic for plain functions, which are the majority."
  (member (treesit-node-type node)
          '("method_declaration" "type_declaration" "type_spec")))

(defun my/lsp-usage-lens--draw (decl-pos refs impls)
  "Draw the lens for the declaration starting at DECL-POS.
IMPLS of nil omits the implementation clause, which is how \"the server
would not answer that\" is distinguished from a genuine zero."
  (save-excursion
    (goto-char decl-pos)
    (let* ((eol (line-end-position))
           (text (format "%d usage%s%s"
                         refs (if (eql refs 1) "" "s")
                         (if impls
                             (format ", %d implementation%s"
                                     impls (if (eql impls 1) "" "s"))
                           "")))
           (ov (make-overlay eol eol nil t t)))
      ;; An `after-string' at end of line, the way CIDER shows evaluation
      ;; results, rather than a `before-string' ending in a newline on the
      ;; line above.
      ;;
      ;; The reason is not taste.  A lens on its own line adds a screen row,
      ;; and the drawn set changes constantly at `window' scope: replies
      ;; arrive in batches (measured: 14 lenses in 5 batches over 483 ms),
      ;; so every batch shifted the viewport by a row.  In dense Go that was
      ;; one visible jump per five keystrokes, and it pushed point off the
      ;; last window line often enough that redisplay scrolled an extra line
      ;; to compensate.  At end of line no row is ever added, so the counts
      ;; can appear and vanish freely without moving a single pixel.
      ;;
      ;; `cursor' keeps the cursor drawn before the annotation rather than
      ;; after it when point sits at end of line.
      ;;
      ;; Residual: with `truncate-lines' nil a declaration whose signature
      ;; is within ~28 columns of the right edge still wraps once the text
      ;; is appended, which is a row appearing.  One of 67 declarations in
      ;; the largest buffer measured, against 12 to 14 churning per pause
      ;; before.  `truncate-lines' t would take it to zero.
      (overlay-put ov 'after-string
                   (concat "  " (propertize text
                                            'face 'my/lsp-usage-lens-face
                                            'display (list 'raise
                                                           (my/lsp-usage-lens--raise-factor))
                                            'cursor t)))
      ;; Keyed by the declaration start, not by `eol', because that is what
      ;; `my/lsp-usage-lens--update' looks up.
      (push (cons decl-pos ov) my/lsp-usage-lens--overlays))))

(defun my/lsp-usage-lens--request (name-pos decl-pos want-impls)
  "Ask for counts at NAME-POS and draw them above DECL-POS.
When WANT-IMPLS is nil the implementation request is not sent at all and
the clause is omitted; see `my/lsp-usage-lens--wants-impls-p'."
  (let ((buf (current-buffer))
        (refs :pending)
        ;; nil rather than `:pending' means render can proceed on refs alone.
        (impls (if want-impls :pending nil)))
    (cl-labels
        ((render ()
           (unless (or (eq refs :pending) (eq impls :pending))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 ;; Point may have moved, or the buffer been edited, while
                 ;; these were in flight.  Only draw if this declaration is
                 ;; still wanted and not already drawn.
                 (when (and (memql decl-pos my/lsp-usage-lens--wanted)
                            (not (assq decl-pos my/lsp-usage-lens--overlays)))
                   (my/lsp-usage-lens--draw decl-pos refs impls)))))))
      (save-excursion
        (goto-char name-pos)
        (lsp-request-async
         "textDocument/references" (lsp--make-reference-params nil t)
         (lambda (res) (setq refs (my/lsp--count-locations res)) (render))
         :mode 'alive
         :error-handler (lambda (_) (setq refs 0) (render)))
        (when want-impls
          (lsp-request-async
           "textDocument/implementation" (lsp--text-document-position-params)
           (lambda (res) (setq impls (my/lsp--count-locations res)) (render))
           :mode 'alive
           ;; nil, not 0: the server declining the question is not an answer.
           :error-handler (lambda (_) (setq impls nil) (render))))))))

(defun my/lsp-usage-lens--targets ()
  "Declaration nodes that should carry a lens right now.
Honours `my/lsp-usage-lens-scope'.

For `window', the test is on each declaration's OWN START position, not
on whether its body overlaps the viewport.  A 200-line function scrolled
so only its middle is visible has nothing to annotate: the lens draws
above the signature line, which is off screen, so an overlapping-body
test would fire two workspace searches to render something invisible.
The declaration containing point is unioned in regardless, which is what
covers exactly that case -- deep inside a long function you still get its
counts, just not the ones you cannot see."
  (pcase my/lsp-usage-lens-scope
    ('window
     (let* ((beg (window-start))
            ;; Plain `window-end': the forced variant simulates display, and
            ;; nothing here needs pixel accuracy.  A declaration one line off
            ;; either edge is a harmless miss, corrected on the next tick.
            (end (or (window-end) (point-max)))
            (here (treesit-defun-at-point))
            (hits '()))
       (dolist (child (my/lsp-usage-lens--all-declarations))
         (let ((start (treesit-node-start child)))
           (when (and (>= start beg) (<= start end))
             (push child hits))))
       (setq hits (nreverse hits))
       ;; Union in the enclosing declaration when its signature is above
       ;; the viewport.
       (if (and here (not (memql (treesit-node-start here)
                                 (mapcar #'treesit-node-start hits))))
           (cons here hits)
         hits)))
    (_ (let ((node (treesit-defun-at-point)))
         (and node (list node))))))

(defun my/lsp-usage-lens--update ()
  "Refresh this buffer's usage lenses if what should be shown has changed."
  ;; Demoted rather than raw: an error raised inside an idle timer repeats
  ;; on every tick and is close to unusable.
  (with-demoted-errors "usage lens: %S"
    (when (and (bound-and-true-p my/lsp-usage-lens-mode)
               (bound-and-true-p lsp-mode)
               (not (minibufferp))
               (fboundp 'treesit-defun-at-point)
               (treesit-parser-list))
      ;; Any edit invalidates both the positions and the counts.
      (unless (eql my/lsp-usage-lens--tick (buffer-chars-modified-tick))
        (my/lsp-usage-lens--clear)
        (setq my/lsp-usage-lens--tick (buffer-chars-modified-tick)))
      (let* ((all (my/lsp-usage-lens--targets))
             (nodes (seq-take all my/lsp-usage-lens-max)))
        (when (> (length all) (length nodes))
          ;; Never truncate silently.
          (message "usage lens: showing %d of %d declarations on screen (see `my/lsp-usage-lens-max')"
                   (length nodes) (length all)))
        (setq my/lsp-usage-lens--wanted
              (mapcar #'treesit-node-start nodes))
        ;; Drop overlays for declarations that scrolled out of scope.
        (dolist (cell (copy-sequence my/lsp-usage-lens--overlays))
          (unless (memql (car cell) my/lsp-usage-lens--wanted)
            (when (overlayp (cdr cell)) (delete-overlay (cdr cell)))
            (setq my/lsp-usage-lens--overlays
                  (delq cell my/lsp-usage-lens--overlays))))
        ;; Request only what is missing, so re-running is idempotent.
        (dolist (node nodes)
          (let ((start (treesit-node-start node))
                (name  (my/lsp-usage-lens--name-node node)))
            (when (and name (not (assq start my/lsp-usage-lens--overlays)))
              (my/lsp-usage-lens--request
               (treesit-node-start name) start
               (my/lsp-usage-lens--wants-impls-p node)))))))))

(define-minor-mode my/lsp-usage-lens-mode
  "Show usage and implementation counts above Go declarations.
Scope is controlled by `my/lsp-usage-lens-scope'.  Prefer
`my/lsp-usage-lens-global-mode' as the switch; this is the per-buffer
mode it drives."
  :lighter " Uses"
  (if my/lsp-usage-lens-mode
      (progn
        (unless my/lsp-usage-lens--timer
          (setq my/lsp-usage-lens--timer
                (run-with-idle-timer my/lsp-usage-lens-idle t
                                     #'my/lsp-usage-lens--update)))
        (add-hook 'change-major-mode-hook #'my/lsp-usage-lens--clear nil t))
    (my/lsp-usage-lens--clear)
    (remove-hook 'change-major-mode-hook #'my/lsp-usage-lens--clear t)))

(defun my/lsp-usage-lens--turn-on ()
  "Enable `my/lsp-usage-lens-mode' where it can do something useful.
Buffers without a tree-sitter parser have no declarations to anchor to;
`lsp-mode' is not checked here because it usually starts after the major
mode does -- `my/lsp-usage-lens--update' rechecks it on every tick."
  (when (and (derived-mode-p 'prog-mode)
             (fboundp 'treesit-parser-list)
             (treesit-parser-list))
    (my/lsp-usage-lens-mode 1)))

;;;###autoload
(define-globalized-minor-mode my/lsp-usage-lens-global-mode
  my/lsp-usage-lens-mode
  my/lsp-usage-lens--turn-on
  :group 'lsp-mode)

(defun my/lsp-usage-lens-set-scope (scope)
  "Set `my/lsp-usage-lens-scope' to SCOPE and redraw every lens buffer.
`at-point' annotates only the declaration containing point: two
language-server searches per declaration you visit, effectively free.
`window' annotates every declaration whose signature is on screen, plus
the one containing point: two searches each, capped by
`my/lsp-usage-lens-max'."
  (interactive
   (list (intern
          (completing-read
           (format "Usage lens scope (currently %s): "
                   my/lsp-usage-lens-scope)
           '("at-point" "window")
           nil t nil nil (symbol-name my/lsp-usage-lens-scope)))))
  (setq my/lsp-usage-lens-scope scope)
  ;; Existing overlays were drawn under the old scope; drop them all so the
  ;; next idle tick rebuilds from scratch rather than leaving orphans.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p my/lsp-usage-lens-mode)
        (my/lsp-usage-lens--clear))))
  (my/lsp-usage-lens--update)
  (message "Usage lens scope: %s%s" scope
           (if (eq scope 'window)
               (format " (up to %d declarations per screen)"
                       my/lsp-usage-lens-max)
             "")))

(with-eval-after-load 'lsp-mode
  (when (boundp 'lsp-command-map)
    (define-key lsp-command-map (kbd "g u") #'my/lsp-usage-counts)
    (define-key lsp-command-map (kbd "T u") #'my/lsp-usage-lens-global-mode)
    (define-key lsp-command-map (kbd "T U") #'my/lsp-usage-lens-set-scope)))

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
  ;; In a Claude buffer, plain `C-c [' EXPORTS the whole conversation to a
  ;; plain-text file and opens it in a read-only Emacs buffer (`q' to quit,
  ;; fully selectable) -- Claude's fullscreen TUI keeps history off-buffer,
  ;; so this is how you reach the FULL history (see
  ;; `my/vterm-copy-or-history' / `my/claude-transcript-to-file').  We use
  ;; the file export, NOT the old scrollback dump, because replaying Claude's
  ;; repainting TUI render into vterm scrollback preserves every intermediate
  ;; repaint frame as history -- so copy mode showed the same blocks twice
  ;; (the "duplicated text in scrollback" bug).  The file is written once, so
  ;; it is clean, and it does not stream every line through vterm.  Use
  ;; `C-u C-c [' to skip the export and copy just the visible screen
  ;; (instant).  Ordinary vterm shells are unaffected -- there `C-c [' is
  ;; plain copy mode as before.
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

;; --- Which Claude Code TUI mode is active (fullscreen vs inline) ---
;;
;; Claude Code has two renderers (`/tui'): `fullscreen' draws on the terminal's
;; ALTERNATE screen (like vim; no scrollback -- the conversation lives in
;; Claude's own state), and `default' (inline) appends to the terminal's NATIVE
;; scrollback.  vterm does not expose which one is active, so we track it here
;; and keep it in step with Claude via `my/claude-toggle-tui'.  A few behaviours
;; below (the mouse wheel, `C-c [') depend on it.
;;
;; DEFAULT is `fullscreen', matching everything this config was built for, so
;; nothing changes until you switch: every mode-dependent branch is inert while
;; this stays `fullscreen'.  To roll back to fullscreen it is enough to set this
;; back to `fullscreen' (or run `my/claude-toggle-tui').
(defvar my/claude-tui-mode 'fullscreen
  "Claude Code TUI mode this config assumes: `fullscreen' or `default'.
Kept in sync with Claude's `/tui' setting by `my/claude-toggle-tui'.  Gates
the mouse-wheel handler and `C-c ['.  Session-local: on a fresh Emacs it
resets to `fullscreen'; if you make inline your permanent Claude mode, also
set this to `default' in your config.")

;; BACKEND-AGNOSTIC on purpose: these two are the only Claude helpers here that
;; are invoked by name (`M-x my/claude-toggle-tui') rather than from a
;; `vterm-mode-hook' or a `vterm-mode-map' key, so unlike everything else above
;; they must keep working after `claude-code-ide-terminal-backend' changes.  They
;; match any of the three terminal modes and send through claude-code-ide's own
;; dispatchers (`claude-code-ide--terminal-send-string' / `--send-return'), which
;; pick the right primitive per backend.
(defun my/claude--terminal-buffer-p ()
  "Non-nil if the current buffer is a Claude Code terminal buffer.
True for any backend: `vterm', `eat' or `ghostel'."
  (and (derived-mode-p 'vterm-mode 'eat-mode 'ghostel-mode)
       (string-match-p "claude-code" (buffer-name))))

(defun my/claude--buffer ()
  "Return a live Claude Code terminal buffer, or nil."
  (or (and (my/claude--terminal-buffer-p) (current-buffer))
      (seq-find (lambda (b)
                  (with-current-buffer b
                    (and (my/claude--terminal-buffer-p)
                         ;; `get-buffer-process' works for all three backends,
                         ;; unlike the vterm-only `vterm--process'.
                         (process-live-p (get-buffer-process b)))))
                (buffer-list))))

(defun my/claude--resume-live-input ()
  "Leave any frozen/read-only view so keystrokes reach Claude again.
vterm calls it copy mode; ghostel has several read-only input modes and
`semi-char' is the universal exit.  No-op when already live."
  (cond
   ((bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1))
   ((and (derived-mode-p 'ghostel-mode)
         (fboundp 'ghostel-semi-char-mode)
         (not (eq (bound-and-true-p ghostel--input-mode) 'semi-char)))
    (ghostel-semi-char-mode))))

(defun my/claude-toggle-tui ()
  "Toggle Claude Code between `fullscreen' and `default' (inline) rendering.
Sends the matching `/tui' command to Claude AND flips `my/claude-tui-mode' so
the Emacs side (mouse wheel, `C-c [') matches.  Run it with the Claude prompt
empty.  Reversible: run it again to switch back."
  (interactive)
  (let ((buf (or (my/claude--buffer)
                 (user-error "No live Claude Code terminal buffer found")))
        (new (if (eq my/claude-tui-mode 'fullscreen) 'default 'fullscreen)))
    (with-current-buffer buf
      (my/claude--resume-live-input)
      (claude-code-ide--terminal-send-string (format "/tui %s" new))
      (claude-code-ide--terminal-send-return))
    (setq my/claude-tui-mode new)
    (message "Claude TUI -> %s.  Emacs wheel/`C-c [' now match %s mode."
             new new)))

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
Scroll the Emacs buffer instead when we are navigating Emacs rather than the
live program: in `vterm-copy-mode' (a read-only view), or when
`my/claude-tui-mode' is `default' (Claude renders inline into vterm's own
scrollback, so the wheel should scroll that scrollback, not be forwarded to a
Claude that is not tracking the mouse)."
  (interactive "e")
  (if (or (bound-and-true-p vterm-copy-mode)
          (eq my/claude-tui-mode 'default))
      ;; Navigating the Emacs buffer: scroll it normally -- smoothly, and
      ;; without erroring at the edges.  NOTE: in Claude's FULLSCREEN mode the
      ;; buffer holds ONLY the current screen, so in copy mode there is nothing
      ;; to scroll until you pull the transcript in (`C-o' then `[').  In
      ;; `default' (inline) mode the scrollback already holds the conversation.
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

;; --- Reliable full-history copy: export the transcript to a FILE ---
;;
;; `my/claude-dump-then-copy' (above) reconstructs history by replaying Claude's
;; conversation into vterm's scrollback (`Ctrl-o' then `['), then reads it in
;; copy mode.  That is fragile: Claude's Ink TUI renders in FRAMES and repaints
;; regions by rewriting lines, and vterm -- a faithful terminal -- commits every
;; intermediate repaint frame to scrollback.  So any block Claude re-emits mid-
;; dump (constant for long code blocks that scroll) lands in scrollback TWICE,
;; and copy mode shows duplicated text.  You cannot dedup it safely either,
;; because code blocks legitimately repeat in a conversation.
;;
;; The clean route is Claude's transcript `v': it writes the WHOLE conversation
;; ONCE, as plain text, to `cc-transcript-<ts>.txt' and opens it through
;; $VISUAL.  Our wrapper (`personal/claude-emacsclient') routes that to
;; `my/claude-view-transcript', a read-only, `q'-to-quit Emacs view.  No repaint
;; frames means no duplicates, it is far faster (no streaming through vterm), and
;; a real Emacs buffer is better for copying anyway (swiper, region kill, etc.).
(defun my/claude-transcript-to-file ()
  "Export Claude's full conversation to a file and open it in Emacs.
Sends transcript-mode `v', so Claude writes the whole conversation to a temp
file and opens it via $VISUAL (`personal/claude-emacsclient'), landing in a
read-only view (`my/claude-view-transcript').  Written once as plain text, so
-- unlike the scrollback dump (`my/claude-dump-then-copy') -- it has no repaint
duplicates."
  (let ((proc (and (boundp 'vterm--process) vterm--process)))
    (unless (and proc (process-live-p proc))
      (user-error "No live Claude process in this buffer"))
    (message "Claude: exporting transcript to a file...")
    (process-send-string proc "\C-o")   ; enter transcript mode
    (accept-process-output proc 0.4)
    (process-send-string proc "v")      ; write transcript file + open in $VISUAL
    (accept-process-output proc 0.4)
    (process-send-string proc "q")      ; leave transcript mode, back to live chat
    (accept-process-output proc 0.2)))

(defun my/vterm-copy-or-history (&optional arg)
  "Toggle `vterm-copy-mode', or (with prefix ARG) export Claude's history.
Plain `C-c [' toggles copy mode over the visible screen + scrollback -- the
fast path, and the useful one in default (inline) mode where vterm's native
scrollback holds the conversation.  (Caveat: in default mode that scrollback
can contain DUPLICATED blocks, because Claude's Ink TUI re-emits any streamed
block taller than the viewport -- the lines that scrolled off the top can no
longer be rewritten in place, so they land in scrollback twice.  Use the
prefix export below when you need clean, duplicate-free history.)

With prefix ARG (`C-u C-c ['), in a Claude buffer, export the whole
conversation to a plain-text file and open it read-only (see
`my/claude-transcript-to-file') -- written ONCE, so no repaint duplicates.
CAVEAT: the export drives Claude's transcript mode (`C-o' then `v'), and the
`v'/`[' export subcommands exist only in the FULLSCREEN renderer.  `C-o'
itself opens a transcript viewer in default (inline) mode too, but there it
offers only `C-e' (show all content) and `q' -- no `v', so this export is a
no-op in default mode.  Switch to fullscreen (`my/claude-toggle-tui') to use it.

In an ordinary (non-Claude) vterm shell, or when already in copy mode, this
just toggles copy mode regardless of ARG."
  (interactive "P")
  (if (and arg
           (not (bound-and-true-p vterm-copy-mode))
           (string-match-p "claude-code" (buffer-name)))
      (my/claude-transcript-to-file)
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
    ;; were away is cleared; see `my/claude-vterm-redraw-on-select'.  Shares
    ;; the single throttle in `my/claude-vterm-redraw', so it can never pair
    ;; with any other C-l into `/clear'.
    (add-hook 'window-selection-change-functions
              #'my/claude-vterm-redraw-on-select nil t)))
(add-hook 'vterm-mode-hook #'my/vterm-let-terminal-own-keys)

;; --- The same, for ghostel ---
;;
;; Only two of the vterm items above still apply to ghostel, because ghostel
;; already does the rest itself in its mode body (`font-lock-mode -1',
;; `buffer-disable-undo', `truncate-lines', `scroll-margin' 0, and `inhibit-quit'
;; so a real C-g reaches the program instead of `keyboard-quit'):
;;
;;   1. `prelude-mode' still wins.  ghostel installs its input maps with
;;      `use-local-map' (`ghostel-semi-char-mode-map' and friends), which is
;;      major-mode level -- and minor-mode maps outrank that, exactly as with
;;      vterm.  Without this, prelude's C-a (crux) and M-o never reach Claude.
;;   2. bidi is pure cost in a terminal.
;;
;; No `char-property-alias-alist' aliasing here: ghostel writes real `face'
;; properties, so it stays colored with font-lock off.  No wheel map either --
;; ghostel forwards the wheel to a mouse-tracking TUI natively.
(defun my/ghostel-let-terminal-own-keys ()
  "Stop `prelude-mode' from shadowing ghostel's own input keymaps."
  (push (cons 'prelude-mode (make-sparse-keymap))
        minor-mode-overriding-map-alist)
  ;; Select-to-copy like a normal terminal emulator.  ghostel additionally
  ;; freezes the buffer on drag release (`ghostel-mouse-drag-input-mode'), so the
  ;; selection survives streaming output; this just also puts it on the kill
  ;; ring, from where clipetty's OSC 52 carries it on tty frames.
  (setq-local mouse-drag-copy-region t)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t))
(add-hook 'ghostel-mode-hook #'my/ghostel-let-terminal-own-keys)

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
  "Turn off `display-line-numbers-mode' in terminal buffers."
  (when (derived-mode-p 'vterm-mode 'ghostel-mode)
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
;; ghostel needs the same treatment for the same reason, but note the ONE
;; difference from vterm: ghostel's native module writes real `face' text
;; properties (it even neutralizes `font-lock-unfontify-region-function' so a
;; globally-forced font-lock cannot strip them), so unlike vterm it needs NO
;; `char-property-alias-alist' aliasing to stay colored with font-lock off.
(setq font-lock-global-modes '(not vterm-mode ghostel-mode))
(with-eval-after-load 'flycheck
  (setq flycheck-global-modes '(not vterm-mode ghostel-mode)))

;; Catch-all safety net: the globalized-mode enablers run from
;; `after-change-major-mode-hook', so append a disable there (runs last,
;; wins) for anything the exclusion lists above miss -- notably the scroll
;; bar, which has no exclusion variable.
(defun my/vterm-disable-heavy-modes ()
  "Disable `after-change-functions' minor modes in terminal buffers."
  (when (derived-mode-p 'vterm-mode 'ghostel-mode)
    (when (bound-and-true-p font-lock-mode) (font-lock-mode -1))
    (when (and (fboundp 'flycheck-mode) (bound-and-true-p flycheck-mode))
      (flycheck-mode -1))
    (when (bound-and-true-p my/scroll-bar-mode)
      (my/scroll-bar-mode -1))))
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

;; --- Repaint a distorted Claude prompt bar (C-l), one shared throttle ---
;;
;; Claude's TUI can leave its bottom-anchored input box drawn at a stale row
;; -- the "distorted prompt bar."  The ROOT CAUSE (libvterm grid vs Claude PTY
;; drift) is fixed by `my/claude-vterm-sync-size' and
;; `my/claude-sync-terminal-dims-fix' (below), so it is now rare; when it does
;; happen, one `C-l' makes Claude re-query its size and FULLY repaint, clearing
;; it.  `C-l' is `chat:clearInput', the only action that does that heavy
;; clear-and-full-repaint (the lighter `app:redraw' does not re-query size --
;; verified).
;;
;; EVERY C-l we send to Claude -- manual (`my/vterm-ctrl-l'), automatic on
;; window select (`my/claude-vterm-redraw-on-select'), and the open-time
;; repaint (`my/claude-sync-terminal-dims-fix') -- goes through the ONE sender
;; below, `my/claude-vterm-redraw', which THROTTLES to at most one send per
;; 2.5s per buffer.  `chat:clearInput' pressed TWICE within 2s is Claude's
;; `/clear'; a single throttled send can never be that pair, no matter how many
;; sources fire or how fast you switch windows.  To actually clear, type
;; `/clear'.
;;
;; A SINGLE `chat:clearInput' does NOT wipe the text you have typed in the
;; prompt box (verified) -- it just triggers the clear-and-full-repaint.  So
;; the on-select redraw is safe to fire freely; its only guard is the throttle,
;; which exists solely to stop two sends pairing into `/clear'.  The bug that
;; started all this was exactly that pairing: several unthrottled C-l sources
;; (on-select + open-time) landing within 2s, which Claude read as `/clear'.
(defvar-local my/claude-vterm-last-redraw 0
  "`float-time' of the last C-l redraw sent to this Claude buffer.")

(defun my/claude-vterm-redraw ()
  "Send one `chat:clearInput' redraw (raw C-l) to Claude, throttled.
At most one send per 2.5s, so it can never be the `C-l' `C-l' within 2s
that Claude reads as `/clear'.  The single shared sender for C-l in a Claude
buffer: used by `my/vterm-ctrl-l', `my/claude-vterm-redraw-on-select', and the
open-time repaint in `my/claude-sync-terminal-dims-fix'."
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
            (when (get-buffer-process buffer)
              ;; Route the open-time repaint through the shared throttle
              ;; (`my/claude-vterm-redraw'), so it can never pair with a manual
              ;; C-l into the `/clear' double-press.
              (run-at-time 0.3 nil
                           (lambda (b)
                             (when (buffer-live-p b)
                               (with-current-buffer b
                                 (my/claude-vterm-redraw))))
                           buffer)))))
    (funcall orig-fn buffer window)))

(with-eval-after-load 'claude-code-ide
  (advice-add 'claude-code-ide--sync-terminal-dimensions
              :around #'my/claude-sync-terminal-dims-fix))

;; --- Terminal backend: ghostel (libghostty) in place of vterm ---
;;
;; ghostel.el drives libghostty-vt, the VT engine behind Ghostty, as a native
;; module.  Three of its capabilities replace hand-written workarounds above:
;;
;;   * DEC 2026 SYNCHRONIZED OUTPUT, which libvterm does not implement.  Claude's
;;     Ink TUI brackets each frame in 2026 begin/end, so a terminal honoring it
;;     never paints a half-finished frame -- the whole class of bug that
;;     `my/claude-vterm-sync-size' and the C-l repaint dance clean up after.
;;     ghostel also cannot hit the grid-vs-PTY desync at all: for this backend
;;     `claude-code-ide--sync-terminal-dimensions' calls BOTH
;;     `ghostel--window-adjust-process-window-size' (the grid) and
;;     `set-process-window-size' (the child), and claude-code-ide leaves its own
;;     #1422 reflow filter disabled for ghostel.
;;
;;   * NATIVE SGR MOUSE FORWARDING.  When a TUI enables DEC mouse tracking (Claude
;;     does), ghostel forwards wheel and click events to the program, so nothing
;;     like `my/vterm-wheel-scroll' is needed.
;;
;;   * SCROLLBACK MATERIALIZED INTO THE BUFFER (`ghostel-max-scrollback', 5 MB),
;;     plus copy mode (`C-c C-t'), copy-everything (`C-c M-w'), and automatic
;;     freezing when a command activates the mark or moves point off the live
;;     cursor (`ghostel-mark-activation-input-mode',
;;     `ghostel-point-leave-input-mode').  So isearch / swiper / region kill work
;;     over history with no transcript export.
;;     CAVEAT: in Claude's `/tui fullscreen' renderer the conversation lives on
;;     the ALTERNATE screen, so no terminal populates scrollback there.  Run
;;     `my/claude-toggle-tui' to put Claude in inline mode if you want the
;;     conversation in the buffer.
;;
;; Everything vterm-specific above stays installed but goes INERT: those hooks
;; run on `vterm-mode-hook', those keys live in `vterm-mode-map', and
;; `my/claude-sync-terminal-dims-fix' already guards on the backend being
;; `vterm'.  Setting `claude-code-ide-terminal-backend' back to `vterm' restores
;; the previous setup with no other edit.
(use-package ghostel
  :ensure t
  :custom
  ;; Keep the native module OUT of the package tree.  A MELPA upgrade rewrites
  ;; elpa/ghostel-*/ and would delete the loaded module out from under a
  ;; long-lived daemon; a stable path survives upgrades.
  (ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory))
  :config
  (unless (file-directory-p ghostel-module-directory)
    (make-directory ghostel-module-directory t))
  ;; tmux-style `C-c [' for copy mode, matching the vterm binding above.
  ;; `ghostel-mode-map' is the base map every input mode inherits (semi-char via
  ;; `set-keymap-parent' in `ghostel--rebuild-semi-char-keymap', copy/Emacs via
  ;; `ghostel-readonly-mode-map's `:parent'), and `ghostel-copy-mode' is itself a
  ;; toggle -- so one binding here works in both directions from every mode.
  ;; ghostel's own `C-c C-t' keeps working.
  (define-key ghostel-mode-map (kbd "C-c [") #'ghostel-copy-mode))

(use-package eat
  :ensure t)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide-insert-at-mentioned)   ;; autoload stub for the wrapper
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("M-i"     . my/claude-add-region-or-tab))
  :custom
  ;; ghostel, not vterm: see the `use-package ghostel' commentary above for why
  ;; (synchronized output, native mouse forwarding, buffer-materialized
  ;; scrollback).  Revert to 'vterm to get the old setup back verbatim.
  (claude-code-ide-terminal-backend 'ghostel)
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

;; --- Manually size / lock / unlock an agent side window ---
;;
;; Covers BOTH integrations: the claude-code-ide terminal side window and the
;; agent-shell (ACP) side window, since both are displayed on the right and the
;; question "how wide should the agent be" is the same either way.
;;
;; Workflow:
;;   1. M-x my/agent-window-set-width    -- set an exact width
;;   2. M-x my/agent-window-fix-width    -- lock it: C-x + (and any other
;;      resize, including mouse dragging) leaves the agent window alone and
;;      balances only the other windows around it
;;   3. M-x my/agent-window-unfix-width  -- unlock: C-x + gives the agent an
;;      equal column share again (the default behavior)
;;
;; The lock uses the built-in buffer-local `window-size-fixed', which every
;; Emacs resize primitive honors.  It lives on the agent BUFFER, so it survives
;; hiding and re-showing the side window, and it is per-buffer -- so a locked
;; claude-code-ide window and an unlocked agent-shell window coexist fine.

(defun my/agent-side-window-p (window)
  "Non-nil if WINDOW is a side window showing an agent buffer.
Recognizes claude-code-ide by buffer name and agent-shell by major mode, which
is sturdier than matching agent-shell's configurable
`agent-shell-buffer-name-format'."
  (and (window-live-p window)
       (window-parameter window 'window-side)
       (let ((buf (window-buffer window)))
         (or (string-prefix-p "*claude-code[" (buffer-name buf))
             (provided-mode-derived-p
              (buffer-local-value 'major-mode buf) 'agent-shell-mode)))))

(defun my/agent-side-window ()
  "Return an agent side window in the selected frame, or nil.
Prefers the selected window when it qualifies, so with both a claude-code-ide
and an agent-shell window open the one you are in is the one you resize."
  (or (and (my/agent-side-window-p (selected-window)) (selected-window))
      (seq-find #'my/agent-side-window-p (window-list nil 'no-minibuffer))))

(defun my/agent-window--require ()
  "Return an agent side window, or signal a `user-error'."
  (or (my/agent-side-window)
      (user-error "No agent side window in this frame")))

(defun my/agent-window-set-width (width)
  "Set the agent side window to WIDTH columns.
Works even while the width is locked with `my/agent-window-fix-width' (the lock
is bypassed for this one resize and stays on afterwards)."
  (interactive
   (list (read-number "Agent window width (columns): "
                      (window-total-width (my/agent-window--require)))))
  (let ((win (my/agent-window--require)))
    (with-current-buffer (window-buffer win)
      (let ((window-size-fixed nil))
        (window-resize win (- width (window-total-width win)) t)))
    (message "%s window width set to %d columns"
             (buffer-name (window-buffer win)) (window-total-width win))))

(defun my/agent-window-fix-width ()
  "Lock the agent side window at its current width.
`C-x +' then balances only the other windows around it."
  (interactive)
  (let ((win (my/agent-window--require)))
    (with-current-buffer (window-buffer win)
      (setq-local window-size-fixed 'width))
    (message "%s window locked at %d columns"
             (buffer-name (window-buffer win)) (window-total-width win))))

(defun my/agent-window-unfix-width ()
  "Unlock the agent side window width.
`C-x +' includes it in balancing again (equal column share)."
  (interactive)
  (let ((win (my/agent-window--require)))
    (with-current-buffer (window-buffer win)
      (setq-local window-size-fixed nil))
    (message "%s window width unlocked" (buffer-name (window-buffer win)))))

;; Old names kept working, so existing muscle memory and any `M-x' history
;; entries still resolve.
(defalias 'claude-code-ide--side-window      #'my/agent-side-window)
(defalias 'claude-code-ide-set-window-width  #'my/agent-window-set-width)
(defalias 'claude-code-ide-fix-window-width  #'my/agent-window-fix-width)
(defalias 'claude-code-ide-unfix-window-width #'my/agent-window-unfix-width)

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
;; 10b. Claude Code over ACP (agent-shell), beside claude-code-ide
;; ============================================================
;;
;; TWO integrations on purpose, because they are good at different things.
;;
;;   * claude-code-ide + ghostel runs the real Claude Code CLI in a terminal.
;;     Full fidelity: `/compact', Esc-Esc transcript rewind, scheduled and
;;     background tasks, `/usage', `/plugin' -- everything the CLI's own TUI
;;     does, because it IS the CLI's own TUI.
;;
;;   * agent-shell speaks ACP (Agent Client Protocol, "LSP but for coding
;;     agents") to `claude-agent-acp', which drives the Claude Agent SDK.  There
;;     is no terminal: the conversation is an ordinary Emacs buffer and the
;;     prompt is ordinary Emacs input.  The part that matters most here is that
;;     Claude's file access is served BY Emacs -- agent-shell implements ACP's
;;     `fs/read_text_file' and `fs/write_text_file', so a read prefers an OPEN
;;     BUFFER (unsaved edits included) and a write lands via
;;     `replace-buffer-contents' in the live buffer plus `basic-save-buffer'.
;;     That means: no auto-revert race, no "buffer has unsaved changes so Emacs
;;     refuses to revert" stand-off (see the autorevert note in section 10), a
;;     minimal marker-preserving edit rather than a whole-file rewrite, and
;;     C-/ undo of anything Claude did.
;;
;; Both integrations talk to the SAME Emacs tool server, configured below, so
;; every tool Claude can call in one it can call in the other.

;; --- Emacs tools over MCP, shared by both integrations ---
;;
;; claude-code-ide ships an MCP (Model Context Protocol) tools server that runs
;; INSIDE Emacs over HTTP -- `claude-code-ide-mcp-http-server.el', bound to
;; 127.0.0.1, on top of the `web-server' package.  `claude-code-ide-emacs-tools-setup'
;; registers five tools into it: xref references, xref apropos, project info,
;; imenu symbols, and tree-sitter info.  We add two more below.
;;
;; ACP itself has NO channel for an agent to call custom client tools.  Its
;; client surface is only `fs/read_text_file', `fs/write_text_file',
;; permissions, terminals and elicitation -- and acp.el advertises just the two
;; fs capabilities.  So the way to give the ACP path these tools is to register
;; the server as an ordinary Claude Code MCP server: `claude-agent-acp' starts
;; the SDK with settingSources ["user" "project" "local"], so a server declared
;; in ~/.claude.json is picked up by the ACP adapter AND by the plain CLI.  One
;; declaration, both integrations, and it keeps working if either is swapped out.
;;
;; It has to be HTTP rather than stdio.  Session-scoped stdio servers handed
;; over ACP's `session/new.mcpServers' currently never reach the model
;; (claude-agent-acp issue #883: registered, but no tools/list, not even shown
;; as known-but-unconnected), whereas the adapter advertises
;; `mcpCapabilities.http' and maps `type: "http"' entries correctly.
;;
;; Register it once, from a shell:
;;
;;   claude mcp add -s user -t http emacs http://127.0.0.1:8765/mcp
;;
;; which is why the port is PINNED here.  The package default is nil, meaning a
;; random port per session -- fine when claude-code-ide injects the URL itself,
;; useless for a static declaration.
(setq claude-code-ide-mcp-server-port 8765)

;; --- Compat: MELPA's `web-server' predates the API claude-code-ide expects ---
;;
;; This is why the Emacs tools were never actually reaching Claude.
;; claude-code-ide's HTTP server calls `ws-process' / `ws-headers' / `ws-body'.
;; Upstream emacs-web-server namespaced its eieio accessors to exactly those
;; names, but the newest MELPA build (web-server 0.1.2, built 2021-07-08, which
;; is what is installed) still declares them un-prefixed as `process' /
;; `headers' / `body'.  So `ws-start' succeeds and the very next line dies with
;; "Symbol's function definition is void: ws-process" -- and because
;; `claude-code-ide-mcp-server--start-server' CATCHES that and only logs a
;; warning, claude-code-ide has been running with NO Emacs tools rather than
;; failing loudly.  Alias the three missing names; the `fboundp' guards make the
;; whole thing a no-op the day MELPA catches up with upstream.
(with-eval-after-load 'web-server
  (dolist (pair '((ws-process . process)
                  (ws-headers . headers)
                  (ws-body    . body)))
    (unless (fboundp (car pair))
      (defalias (car pair) (cdr pair)))))

(defun my/mcp--user-buffer ()
  "Return the buffer the user is most plausibly looking at.
`buffer-list' is in most-recently-used order and is reordered by window
selection, NOT by `set-buffer'/`with-current-buffer' -- so the first
file-visiting buffer that is also displayed stays a reliable answer even though
this runs inside the MCP server's process filter, where `current-buffer' is
whatever Emacs happened to leave current."
  (or (seq-find (lambda (b)
                  (and (buffer-file-name b) (get-buffer-window b t)))
                (buffer-list))
      (seq-find #'buffer-file-name (buffer-list))))

;; --- Make the built-in tools follow your attention ---
;;
;; claude-code-ide's own five tools run inside
;; `claude-code-ide-mcp-server-with-session-context', which ERRORS outright
;; ("No session context found for session nil") unless the request arrived on a
;; per-session URL like /mcp/<session-id> that claude-code-ide registered when it
;; launched a CLI session.  A static ~/.claude.json declaration cannot carry
;; that, and a hardcoded session id could only ever pin ONE project.
;;
;; So supply the context live instead: when no session is registered, synthesize
;; one from the buffer the developer is looking at.  For a single long-lived
;; daemon this is better than a pinned project directory anyway, because
;; `project-info' and the xref tools then track where you actually are rather
;; than where the session happened to start.  claude-code-ide's own sessions
;; return a real context, so for those this advice never fires.
(defun my/mcp--fallback-session-context (result)
  "Filter-return advice: synthesize an MCP session context when RESULT is nil."
  (or result
      (when-let ((buf (my/mcp--user-buffer)))
        (list :project-dir (with-current-buffer buf
                             (or (and (fboundp 'projectile-project-root)
                                      (ignore-errors (projectile-project-root)))
                                 default-directory))
              :buffer buf
              :last-active-buffer buf))))

(with-eval-after-load 'claude-code-ide-mcp-server
  (advice-add 'claude-code-ide-mcp-server-get-session-context
              :filter-return #'my/mcp--fallback-session-context))

(defun my/mcp-emacs-context ()
  "Report what the user currently has open, selected, and in flight.
This is the PULL replacement for claude-code-ide's push-based `selection_changed'
notification.  ACP has no equivalent (its `document/didFocus' family belongs to
the inline-completion surface, not to a chat turn), so instead of streaming
cursor moves at Claude we let Claude ask when it actually matters."
  (let ((buf (my/mcp--user-buffer)))
    (if (not buf)
        "No file-visiting buffer is currently open in Emacs."
      (with-current-buffer buf
        (let* ((win (get-buffer-window buf t))
               (pos (if win (window-point win) (point)))
               (others (seq-uniq
                        (delq nil (mapcar (lambda (w)
                                            (buffer-file-name (window-buffer w)))
                                          (window-list nil 'no-mini)))))
               (diags (ignore-errors
                        (require 'claude-code-ide-diagnostics)
                        (claude-code-ide-diagnostics-get-all buf))))
          (string-join
           (delq nil
                 (list
                  (format "Current file: %s" (or (buffer-file-name) (buffer-name)))
                  (format "Major mode: %s" major-mode)
                  (format "Cursor: line %d, column %d (of %d lines)"
                          (line-number-at-pos pos)
                          (save-excursion (goto-char pos) (current-column))
                          (line-number-at-pos (point-max)))
                  (when (buffer-modified-p)
                    (concat "NOTE: this buffer has UNSAVED changes. Reads through "
                            "the editor already see them; read the file rather "
                            "than assuming disk contents."))
                  (when (use-region-p)
                    (format "Active selection, lines %d-%d:\n```\n%s\n```"
                            (line-number-at-pos (region-beginning))
                            (line-number-at-pos (region-end))
                            (buffer-substring-no-properties
                             (region-beginning)
                             (min (region-end) (+ (region-beginning) 4000)))))
                  (when (cdr others)
                    (format "Other files visible on screen: %s"
                            (string-join (delete (buffer-file-name) others) ", ")))
                  (when (and diags (> (length diags) 0))
                    (format "This file has %d diagnostic(s); call emacs_diagnostics for them."
                            (length diags)))
                  (format "Working directory: %s" default-directory)))
           "\n"))))))

(defun my/mcp-diagnostics (&optional file_path)
  "Return the diagnostics Emacs has ALREADY computed for FILE_PATH.
Defaults to the buffer the user is looking at.  Reuses claude-code-ide's backend
abstraction, which auto-detects flycheck or flymake per buffer -- this config
runs flycheck nearly everywhere and flymake-ruff in Python, so both matter.
Cheaper and more faithful than asking Claude to shell out to a linter, because
these results already reflect the UNSAVED buffer."
  (require 'claude-code-ide-diagnostics)
  (let ((buf (if (and file_path (not (string-empty-p file_path)))
                 (find-file-noselect file_path)
               (my/mcp--user-buffer))))
    (if (not (buffer-live-p buf))
        "No such buffer or file."
      (let ((diags (append (claude-code-ide-diagnostics-get-all buf) nil))
            (name (buffer-file-name buf)))
        (if (null diags)
            (format "No diagnostics for %s." (or name (buffer-name buf)))
          (mapconcat
           (lambda (d)
             (let ((start (map-nested-elt d '(range start))))
               (format "%s:%s:%s: %s: %s [%s]"
                       (or name (buffer-name buf))
                       (alist-get 'line start) (alist-get 'character start)
                       (alist-get 'severity d) (alist-get 'message d)
                       (alist-get 'source d))))
           diags "\n"))))))

(defun my/mcp-register-emacs-tools ()
  "Start the shared Emacs MCP tools server and register every tool on it.
Idempotent: `claude-code-ide-make-tool' registers via `add-to-list', so
re-running only re-adds specs that actually changed."
  (interactive)
  (require 'claude-code-ide)
  (require 'claude-code-ide-mcp-server)
  ;; `claude-code-ide-mcp-xref-find-apropos' calls `apropos-parse-pattern', which
  ;; apropos.el does NOT autoload (only `apropos' itself carries a cookie).  In a
  ;; session that has never run `C-h a' the tool therefore fails with "Symbol's
  ;; function definition is void".  Load it up front.
  (require 'apropos)
  ;; Registers the five built-ins AND flips `claude-code-ide-enable-mcp-server',
  ;; which `claude-code-ide-mcp-server-ensure-server' gates on.
  (claude-code-ide-emacs-tools-setup)
  (claude-code-ide-make-tool
   :function #'my/mcp-emacs-context
   :name "emacs_context"
   :description
   (concat "What the developer is looking at in their editor RIGHT NOW: the "
           "current file, cursor position, any active text selection, other "
           "files visible on screen, and whether the buffer has unsaved edits. "
           "Call this when the user says \"this file\", \"here\", \"this "
           "function\", or otherwise refers to their editor state without "
           "naming a path.")
   :args nil)
  (claude-code-ide-make-tool
   :function #'my/mcp-diagnostics
   :name "emacs_diagnostics"
   :description
   (concat "Linter and language-server diagnostics that the editor has already "
           "computed for a file, including for unsaved changes. Prefer this "
           "over running a linter yourself: it is instant and it reflects the "
           "buffer, not the file on disk.")
   :args '((:name "file_path"
                  :type string
                  :optional t
                  :description "Absolute path; defaults to the file the developer is viewing")))
  (or (claude-code-ide-mcp-server-ensure-server)
      (user-error "Could not start the Emacs MCP tools server on port %s"
                  claude-code-ide-mcp-server-port)))

;; Start it once the daemon is up, slightly deferred so it never sits on the
;; critical path of the first frame.  It must be listening BEFORE Claude
;; connects, because the ~/.claude.json declaration is static: nothing spins the
;; server up on demand the way claude-code-ide does for its own sessions.
(add-hook 'after-init-hook
          (lambda ()
            (run-at-time 2 nil (lambda ()
                                 (ignore-errors (my/mcp-register-emacs-tools))))))

;; --- agent-shell ---
(use-package agent-shell
  :ensure t
  ;; A long list on purpose.  agent-shell carries `;;;###autoload' cookies on
  ;; only a handful of its commands (`agent-shell', `agent-shell-toggle',
  ;; `agent-shell-new-shell', `agent-shell-fork', `agent-shell-restart',
  ;; `agent-shell-resume-session', `agent-shell-prompt-compose' and the
  ;; per-vendor starters), so on a cold daemon `M-x agent-shell-switch-buffer'
  ;; and most of the rest simply would not be found until something else pulled
  ;; the package in.  Everything named here gets an autoload stub, so `M-x'
  ;; completion offers it from the first frame.
  :commands (agent-shell-anthropic-start-claude-code
             agent-shell-buffers agent-shell-switch-buffer agent-shell-other-buffer
             agent-shell-interrupt agent-shell-status
             agent-shell-send-dwim agent-shell-send-region agent-shell-send-region-to
             agent-shell-send-current-file agent-shell-send-file agent-shell-send-other-file
             agent-shell-quote-region agent-shell-insert-file
             agent-shell-insert-shell-command-output
             agent-shell-send-clipboard-image agent-shell-send-screenshot
             agent-shell-set-session-mode agent-shell-cycle-session-mode
             agent-shell-set-session-model agent-shell-set-session-thought-level
             agent-shell-show-usage agent-shell-prompt-queue
             agent-shell-open-transcript agent-shell-copy-as-markdown
             agent-shell-copy-session-id agent-shell-copy-source-block-at-point
             agent-shell-narrow-to-block
             agent-shell-diff-accept-all agent-shell-diff-reject-all
             agent-shell-view-acp-logs agent-shell-toggle-logging agent-shell-version)
  :custom
  ;; Same slot as the claude-code-ide side window, so the window commands
  ;; further down (`my/balance-windows-including-side',
  ;; `claude-code-ide-set-window-width' and the width lock) apply unchanged and
  ;; the two integrations never fight over screen real estate.
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 100)))
  ;; On start, offer the resumable sessions for this project alongside "new".
  ;; This is the conversation switcher: the adapter advertises session/list,
  ;; session/resume, session/load and session/fork, so all of it is real.
  (agent-shell-session-strategy 'prompt)
  ;; Replay the WHOLE conversation back into the buffer when resuming, via
  ;; `session/load'.  Costs a beat on restore, and buys the thing that matters
  ;; here: isearch / swiper / consult-line and region-kill work over the entire
  ;; history the moment a session is back, not just over what arrives next.
  (agent-shell-session-restore-verbosity 'full)
  ;; Permission mode for new sessions.  `default' is Claude Code's "Manual":
  ;; prompt before edits and before anything dangerous.  The alternatives the
  ;; adapter advertises are `acceptEdits', `auto', `plan', `dontAsk' and
  ;; (when permitted) `bypassPermissions'; `C-c C-m' switches per session and
  ;; `C-<tab>' cycles, so this is only the starting point.
  (agent-shell-anthropic-default-session-mode-id "default")
  :config
  ;; Same reasoning as the terminal buffers: a long conversation buffer is
  ;; rewritten constantly while streaming, and flycheck / yascroll hooked into
  ;; `after-change-functions' there is pure cost.  Font-lock stays ON, unlike in
  ;; a terminal -- here it is what renders the markdown.
  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-global-modes 'agent-shell-mode t)))

(defun my/agent-shell-live-p ()
  "Non-nil when at least one agent-shell buffer exists."
  (and (fboundp 'agent-shell-buffers)
       (featurep 'agent-shell)
       (agent-shell-buffers)))

;; --- Transcripts in both places ---
;;
;; agent-shell writes every conversation to <project>/.agent-shell/transcripts/
;; as it happens, which keeps a transcript next to the code it discusses but
;; makes "search everything I have ever asked Claude" a walk over many roots.
;; So also collect them centrally -- as a HARD LINK, not a copy: the mirror
;; shares the file's inode, so it stays current while the conversation streams
;; in, with no duplicated bytes, no timer and nothing to keep in sync.
;;
;; Remember `git check-ignore' hygiene: add `.agent-shell/' to your global
;; gitignore, or every repo grows an untracked directory.
(defvar my/agent-shell-transcript-mirror-dir
  (expand-file-name "agent-shell-transcripts/" user-emacs-directory)
  "Directory holding a hard link to every agent-shell transcript.
Flat, so one `rg PATTERN' over it searches every conversation in every
project.  Not indexed by `agent-recall' -- see its `search-paths' below.")

(defun my/agent-shell-mirror-transcript (path)
  "Hard-link transcript PATH into `my/agent-shell-transcript-mirror-dir'.
`:filter-return' advice on `agent-shell--ensure-transcript-file', so PATH is
passed through untouched and the advised function's contract is preserved.
Falls back to a symlink when the two ends are on different filesystems."
  (when (and path (file-exists-p path))
    (ignore-errors
      (unless (file-directory-p my/agent-shell-transcript-mirror-dir)
        (make-directory my/agent-shell-transcript-mirror-dir t))
      ;; Name the link <project>-<timestamp>.md: every project stamps its
      ;; transcripts with the same format, so the bare names would collide.
      ;; PATH is <project>/.agent-shell/transcripts/<stamp>.md, hence "../../".
      (let* ((project (file-name-nondirectory
                       (directory-file-name
                        (expand-file-name "../../" (file-name-directory path)))))
             (link (expand-file-name
                    (format "%s-%s" project (file-name-nondirectory path))
                    my/agent-shell-transcript-mirror-dir)))
        (unless (file-exists-p link)
          (condition-case nil
              (add-name-to-file path link)
            (error (make-symbolic-link path link t)))))))
  path)

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--ensure-transcript-file
              :filter-return #'my/agent-shell-mirror-transcript))

;; --- agent-shell companions ---

(use-package agent-recall
  :ensure t
  :after agent-shell
  :custom
  ;; Roots scanned for <project>/.agent-shell/transcripts/.  Deliberately NOT
  ;; `my/agent-shell-transcript-mirror-dir': agent-recall would index both a
  ;; transcript and its hard link and report every hit twice.  The mirror is for
  ;; one-shot `rg' from a shell; agent-recall reads the originals.
  (agent-recall-search-paths '("~/professional/" "~/.emacs.d/"))
  :commands (agent-recall-search agent-recall-search-live
             agent-recall-browse agent-recall-reindex))

(use-package agent-shell-attention
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el" :rev :newest)
  :after agent-shell
  :config
  ;; Mode-line indicator for which shells are waiting on a permission answer.
  ;; With Manual permissions this is the thing that tells you a session is
  ;; blocked without having to keep its window on screen.
  (agent-shell-attention-mode 1))

(use-package agent-shell-manager
  :vc (:url "https://github.com/jethrokuan/agent-shell-manager" :rev :newest)
  :commands (agent-shell-manager-toggle))

;; --- Desktop notifications, with a macOS backend ---
;;
;; agent-shell-notifications ships two providers: libnotify (D-Bus, so Linux
;; only) and an experimental knockknock one that is not on MELPA and needs an
;; unmerged upstream PR.  Neither works here.  Its backend contract is just four
;; variables (see agent-shell-notifications-libnotify.el), so supply an
;; `osascript' provider: no extra dependency, native banners.
;;
;; The trade-off: `osascript' can post a banner but cannot dismiss one or carry
;; a click action, so `close' is a no-op and the `agent-shell-attention'
;; mode-line indicator above stays the thing you actually click.  Installing
;; `terminal-notifier' (`brew install terminal-notifier') would restore both
;; through its -group / -remove / -execute flags if that ever matters.
;;
;; Strings go to `osascript' as ARGV, never interpolated into the script, so
;; quotes, backslashes and newlines in a tool name or error message cannot
;; break or inject into the AppleScript.
(defconst my/osascript-notify-program
  "on run argv\ndisplay notification (item 1 of argv) with title (item 2 of argv)\nend run"
  "AppleScript that posts its two arguments as a notification.")

(defun my/agent-shell-notify-macos (plist)
  "Post PLIST, a `notifications-notify' style plist, as a macOS banner."
  (ignore-errors
    (start-process "agent-shell-notify" nil "osascript"
                   "-e" my/osascript-notify-program
                   (or (plist-get plist :body) "")
                   (or (plist-get plist :title) "agent-shell")))
  'my/osascript-notification)

;; Installed by hand into site-lisp rather than with `:vc', because
;; `package-vc' unions the `Package-Requires' of EVERY .el in the repo into one
;; descriptor -- and the optional `agent-shell-notifications-knockknock.el'
;; backend declares `(agent-shell-notifications "0.1")' and `(knockknock
;; "0.3")'.  That yields a package that depends on ITSELF (install blows the
;; Lisp nesting limit) and on knockknock, which is not on MELPA.  The main file
;; needs only `agent-shell', so a plain `:load-path' sidesteps the descriptor
;; entirely.  `git -C ~/.emacs.d/site-lisp/agent-shell-notifications pull' to
;; update; the knockknock backend is deleted locally, being unusable here.
(use-package agent-shell-notifications
  :load-path "site-lisp/agent-shell-notifications"
  :after agent-shell
  :hook (agent-shell-mode . agent-shell-notifications-mode)
  :config
  (setq agent-shell-notifications-send-function #'my/agent-shell-notify-macos
        agent-shell-notifications-close-function #'ignore
        agent-shell-notifications-transform-function #'identity
        agent-shell-notifications-transform-timeout-function #'identity))

;; --- M-i: send the region to whichever Claude is running ---
;;
;; Prefers agent-shell when one is live, because `agent-shell-send-dwim' is
;; strictly the better version of this action: the region arrives as a clickable
;; `file:line-start-line-end' reference rather than a bare @-mention, it falls
;; back to the flycheck/flymake error at point when there is no region, and it
;; QUEUES the text if Claude is mid-turn instead of dropping it.
(defun my/claude-add-region-or-tab ()
  "Region active: send it to Claude.  No region: `tab-to-tab-stop'.
Targets a live agent-shell if there is one, else a claude-code-ide session."
  (interactive)
  (cond
   ((not (use-region-p))
    (call-interactively #'tab-to-tab-stop))
   ((my/agent-shell-live-p)
    (call-interactively #'agent-shell-send-dwim))
   (t
    (call-interactively #'claude-code-ide-insert-at-mentioned))))

;; --- agent-shell command prefix ---
;;
;; `C-c C-;', deliberately shaped like the `C-c C-'' that opens the
;; claude-code-ide menu: two punctuation chords, neither one a common
;; major-mode binding, so nothing shadows them in a code buffer.
(defvar my/agent-shell-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") #'agent-shell-anthropic-start-claude-code)
    (define-key m (kbd "b") #'agent-shell-switch-buffer)
    (define-key m (kbd "n") #'agent-shell-new-shell)
    ;; Branch the conversation from here into a new shell, leaving this one
    ;; intact.  The nearest thing ACP offers to the CLI's Esc-Esc rewind, which
    ;; the adapter does not expose yet (issue #583).
    (define-key m (kbd "f") #'agent-shell-fork)
    (define-key m (kbd "r") #'agent-shell-resume-session)
    ;; Every session is also written to <project>/.agent-shell/transcripts/*.md
    ;; as it happens, so past conversations are grep-able across projects.
    (define-key m (kbd "t") #'agent-shell-open-transcript)
    (define-key m (kbd "c") #'agent-shell-prompt-compose)
    (define-key m (kbd "s") #'agent-shell-status)
    ;; Companions: tabulated view of every shell, and transcript search.
    (define-key m (kbd "l") #'agent-shell-manager-toggle)
    (define-key m (kbd "/") #'agent-recall-search-live)
    (define-key m (kbd "R") #'agent-recall-browse)
    m)
  "Keymap for agent-shell commands, bound to `C-c C-;'.")

(global-set-key (kbd "C-c C-;") my/agent-shell-map)

;; ============================================================
;; 10c. Debugging (Debug Adapter Protocol via dape)
;; ============================================================
;;
;; This is the one capability GoLand had that had no counterpart here at
;; all.  dape speaks the Debug Adapter Protocol, the debugging equivalent
;; of LSP, and ships a ready-made Delve configuration for `go-mode' and
;; `go-ts-mode' -- so `M-x dape' offers Go entries with no per-project
;; setup.  It also covers Python (debugpy) and Rust (codelldb) if those
;; adapters are installed later.
;;
;; Requires `dlv' on PATH (installed).  Everything hangs off dape's own
;; prefix, `C-x C-a', which is free here and works in a terminal frame
;; (`C-c d' is not usable: prelude-mode-map binds it to
;; `crux-duplicate-current-line-or-region', and a minor-mode map outranks
;; the global map).  Typical loop:
;;
;;   C-x C-a b   toggle a breakpoint on this line
;;   C-x C-a d   start a session, then pick `dlv' or `dlv-test'
;;   C-x C-a n / s / o   step over / into / out
;;   C-x C-a c   continue
;;   C-x C-a i   info buffers: scopes, watch, stack, breakpoints, threads
;;   C-x C-a x   evaluate an expression in the stopped frame
;;   C-x C-a w   watch the thing at point
;;   C-x C-a e   conditional breakpoint (an expression that must hold)
;;   C-x C-a h   hit-count breakpoint
;;   C-x C-a R   repl
;;   C-x C-a q   quit the session
;;
;; `dape-info' is the closest equivalent to GoLand's debugger panel, and
;; `dape-select-thread' switches goroutines.
(use-package dape
  :ensure t
  :init
  ;; Side-by-side panels rather than stacked, matching how magit and lsp
  ;; definitions are set up elsewhere in this file.
  (setq dape-buffer-window-arrangement 'right)
  :custom
  ;; Annotate the code with live variable values while stopped, the way
  ;; GoLand does during a debug session.  (This is dape's default; stated
  ;; explicitly because it is one of the things being matched.)
  (dape-inlay-hints t)
  :config
  ;; Keep the compile buffer from lingering once the build succeeds.
  (add-hook 'dape-compile-hook #'kill-buffer)
  ;; Clickable breakpoint controls in the fringe, in every prog-mode
  ;; buffer rather than only during a session.
  (dape-breakpoint-global-mode 1)

  ;; dape ships a `dlv' entry (modes go-mode and go-ts-mode) that launches
  ;; the package in the current directory.  It has no test counterpart, so
  ;; add one: `:mode "test"' is how the Delve DAP adapter is told to build
  ;; and run the package's tests.  This is GoLand's "Debug test".
  (add-to-list 'dape-configs
               '(dlv-test
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd (file-name-directory (buffer-file-name))
                 command-insert-stderr t
                 port :autoport
                 :request "launch"
                 :type "go"
                 :mode "test"
                 :cwd "."
                 :program ".")))

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

;; --- Formatting on save: gofumpt + goimports, both through gopls ------
;;
;; gopls performs both jobs itself, so neither binary is spawned per save:
;;
;;   * `lsp-go-use-gofumpt' makes textDocument/formatting apply gofumpt's
;;     rules.  gofumpt is linked into gopls as a library.
;;   * source.organizeImports IS the goimports engine
;;     (golang.org/x/tools/internal/imports) driven over LSP.  It adds,
;;     drops and regroups imports against the already-loaded package graph.
;;
;; `prelude-go-mode-defaults' additionally installs `gofmt-before-save'.
;; That was merely redundant while goimports was absent; now that
;; goimports is on PATH, prelude points `gofmt-command' at it -- and
;; goimports applies plain gofmt rules, which quietly undo the extra
;; normalisations gofumpt just made.  The two would disagree on every
;; save, so `my/go-drop-prelude-gofmt' below removes prelude's hook and
;; leaves gopls as the single formatter.

(defun my/go-lsp-format-on-save ()
  "Format the Go buffer with gopls, using gofumpt rules."
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (lsp-format-buffer)))

(defun my/go-lsp-organize-imports ()
  "Organize Go imports with gopls, using the goimports engine."
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (lsp-organize-imports)))

(defun my/go-lsp-setup ()
  "Install this buffer's Go save hooks: organize imports, then format."
  ;; Explicit depths rather than bare `add-hook', so the order is stated
  ;; rather than inherited from the order these two calls happen to run
  ;; in: imports first (that rewrites the import block), formatting second
  ;; (that normalises whatever the rewrite produced).
  (add-hook 'before-save-hook #'my/go-lsp-organize-imports -10 t)
  (add-hook 'before-save-hook #'my/go-lsp-format-on-save    10 t))

(defun my/go-drop-prelude-gofmt ()
  "Remove prelude's `gofmt-before-save' so gopls is the only formatter."
  (remove-hook 'before-save-hook #'gofmt-before-save t))

;; Depth 95: prelude adds its hook from `go-ts-mode-hook', so this has to
;; run after it.
(add-hook 'go-ts-mode-hook #'my/go-drop-prelude-gofmt 95)
(add-hook 'go-mode-hook    #'my/go-drop-prelude-gofmt 95)

;; --- Keep super-save away from Go buffers -----------------------------
;;
;; `super-save-mode' is on (core/prelude-editor.el) and saves on window
;; and buffer switches.  With the hooks above, every one of those saves
;; reorganises imports and reformats -- so glancing at another window
;; mid-edit can strip an import for a call you have not finished typing
;; yet.  prelude-go excludes `go-mode' from `super-save-predicates', but
;; this config remaps .go files to `go-ts-mode', which the exclusion
;; misses.  A predicate returning nil blocks the save (`super-save-p'
;; requires every predicate to pass).
(with-eval-after-load 'super-save
  (add-to-list 'super-save-predicates
               (lambda () (not (derived-mode-p 'go-mode 'go-ts-mode)))))

;; --- Stop drawing indentation tabs in Go buffers ---
;;
;; `prelude-go-mode-defaults' (core prelude, modules/prelude-go.el) calls
;; (whitespace-toggle-options '(tabs)), which buffer-locally pushes `tabs'
;; onto `whitespace-active-style'.  Every indentation tab then gets the
;; `whitespace-tab' face -- which section 9 styles as a dim grey underline.
;; Go indents with tabs by design, so the result is a long horizontal rule
;; running through the indentation of every nested line.
;;
;; Restarting whitespace-mode makes it re-read the global `whitespace-style'
;; (section 9), which deliberately leaves `tabs' out.  Makefiles keep their
;; tab highlighting, where a tab-vs-space mixup is an actual syntax error.
(defun my/go-untoggle-tab-visualization ()
  "Undo prelude-go's tab highlighting in this Go buffer."
  (when (and (bound-and-true-p whitespace-mode)
             (memq 'tabs whitespace-active-style))
    (whitespace-mode -1)
    (whitespace-mode +1)))

;; Depth 90: must run *after* prelude's own go hook, which is what turns
;; tab visualization on in the first place.
(add-hook 'go-ts-mode-hook #'my/go-untoggle-tab-visualization 90)
(add-hook 'go-mode-hook    #'my/go-untoggle-tab-visualization 90)

(use-package go-ts-mode
  :mode ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . lsp-deferred)
  (go-ts-mode . my/go-lsp-setup)
  :config
  ;; gopls settings
  (setq lsp-go-use-gofumpt t)
  (setq lsp-go-staticcheck t)

  ;; Analyzers.  gopls ships far more than the two that were enabled; each
  ;; of these is an inspection GoLand runs by default.  `fieldalignment'
  ;; is left off deliberately: it flags almost every struct in a real
  ;; codebase and the payoff is padding bytes.
  (setq lsp-go-analyses '((unusedparams   . t)
                          (shadow         . t)
                          (nilness        . t)   ; nil deref, impossible conditions
                          (unusedwrite    . t)   ; value assigned, never read
                          (useany         . t)   ; interface{} -> any
                          (unusedvariable . t)
                          (fieldalignment . :json-false)))

  ;; Hover.  The default "SynopsisDocumentation" shows the first sentence
  ;; only; GoLand's Ctrl+Q shows the whole doc comment.
  (setq lsp-go-hover-kind "FullDocumentation")

  ;; text/template and html/template files get gopls' template support,
  ;; which GoLand has and plain go-ts-mode does not.
  (setq lsp-go-template-extensions ["tmpl" "gotmpl" "gohtml"])

  ;; Import grouping.  Set this to the module prefix (the first path
  ;; element of `module' in go.mod, e.g. "github.com/unifize") and gopls
  ;; will keep company imports in their own block, the way
  ;; `goimports -local' does.  Left empty until the prefix is confirmed.
  ;; (setq lsp-go-goimports-local "your.module/prefix")

  ;; Inlay hints are configured below, outside this block -- lsp-go.el has
  ;; no variables for them, so they need registering by hand.

  ;; --- Test runner ---
  ;;
  ;; prelude-go installs these on `go-mode-map', but .go files are remapped
  ;; to `go-ts-mode' (section 3), and `go-ts-mode-map' descends from
  ;; `prog-mode-map', not from `go-mode-map'.  The result was an installed
  ;; `gotest' package with no reachable keys at all.  Rebound here.
  ;;
  ;; All of these shadow global bindings only inside Go buffers, since a
  ;; major-mode map outranks the global map.
  (define-key go-ts-mode-map (kbd "C-c a")   #'go-test-current-project)
  (define-key go-ts-mode-map (kbd "C-c m")   #'go-test-current-file)
  (define-key go-ts-mode-map (kbd "C-c .")   #'go-test-current-test)
  (define-key go-ts-mode-map (kbd "C-c b")   #'go-run)
  (define-key go-ts-mode-map (kbd "C-c C-v") #'go-test-current-coverage)
  (define-key go-ts-mode-map (kbd "C-c C-b") #'go-test-current-benchmark)
  ;; GoLand's Ctrl+Shift+T: jump between foo.go and foo_test.go.
  (define-key go-ts-mode-map (kbd "C-c C-t")
              #'projectile-toggle-between-implementation-and-test))
;; Note: `C-c C-d' is left alone -- go-ts-mode binds it to
;; `go-ts-mode-docstring', which inserts a doc comment stub.  For symbol
;; documentation use `C-c L h h' (lsp-describe-thing-at-point); it comes
;; from gopls and covers dependencies, which `godoc-at-point' does not.

;; --- Inlay hints ------------------------------------------------------
;;
;; `lsp-inlay-hint-enable' (section 10) makes lsp-mode ASK for hints; this
;; is what tells gopls which ones to produce.
;;
;; The `lsp-go-inlay-hints-*' variables that used to live in the block
;; above do not exist.  lsp-go.el registers 23 "gopls.*" settings and
;; `hints' is not among them (grep it: the file mentions "inlay" twice,
;; both about server capabilities), so those four setq calls were creating
;; four global variables that nothing ever read.  Registering the setting
;; by hand is the supported route -- `lsp-register-custom-settings' is the
;; same mechanism lsp-go.el itself uses, and an alist of (symbol . t)
;; serialises to a JSON object exactly like `lsp-go-analyses' does.
;;
;; Names and behaviour verified against the installed gopls via
;; `gopls api-json'.  All eight default to off.  The « » markers below are
;; gopls' own notation for where the hint text is drawn.
(defvar lsp-go-hints
  '(;; parseInt(« str: » "123", « radix: » 8)
    (parameterNames         . t)
    ;; i« int», j« int» := 0, len(r)-1
    (assignVariableTypes    . t)
    ;; for k« int», v« string» := range []string{} {
    (rangeVariableTypes     . t)
    ;; Point2D{«X: »1, «Y: »2}
    (compositeLiteralFields . t)
    ;; const ( KindNone Kind = iota« = 0» ; KindPrint«  = 1» )
    (constantValues         . t)
    ;; myFoo«[int, string]»(1, "hello")
    (functionTypeParameters . t)

    ;; Off by design, not oversight:
    ;;
    ;; compositeLiteralTypes annotates anonymous struct types inside
    ;; composite literals.  In table-driven tests -- which is most Go test
    ;; code -- that repeats the whole struct type on every case.
    (compositeLiteralTypes  . :json-false)
    ;; ignoredError appends "// ignore error" after every implicitly
    ;; discarded error.  Genuinely useful for auditing, and genuinely
    ;; noisy in code with many `defer f.Close()' lines.  Worth flipping on
    ;; for a session when hunting swallowed errors.
    (ignoredError           . :json-false))
  "Inlay hints gopls should produce.  Sent as the gopls `hints' setting.")

(with-eval-after-load 'lsp-mode
  (lsp-register-custom-settings '(("gopls.hints" lsp-go-hints))))

;; --- golangci-lint ----------------------------------------------------
;;
;; This is the closest thing to GoLand's inspection set: golangci-lint
;; aggregates ~50 linters, well past what `go vet' plus the staticcheck
;; gopls embeds will report.
;;
;; The MELPA package `flycheck-golangci-lint' is deliberately NOT used.
;; It was last touched in 2019 and invokes `--out-format=checkstyle',
;; a flag golangci-lint v2 removed outright (v2.12 here; the spelling is
;; now `--output.checkstyle.path').  The installed binary rejects the old
;; flag, so that package cannot work.  This is the same idea against the
;; v2 command line.
;;
;; Scope is `.', the current package, not `./...'.  Whole-module linting
;; on every check is far too slow to sit in the edit loop; use
;; `my/go-golangci-lint-project' below for the full sweep.
;;
;; `--allow-serial-runners' is what keeps this usable.  golangci-lint
;; takes a machine-global file lock in `os.TempDir()' on startup, and by
;; default a second instance that cannot get the lock exits 3 with
;; "parallel golangci-lint is running".  Flycheck then reports
;; "Suspicious state ... returned 3, but its output contained no errors"
;; and disables the checker.  Two Go buffers checking at once, or a
;; flycheck run overlapping `my/go-golangci-lint-project', is enough to
;; trigger it.  `--allow-serial-runners' makes the loser wait on the lock
;; instead of failing.  Not `--allow-parallel-runners', which drops
;; locking entirely and lets concurrent runs contend over the build cache.
(with-eval-after-load 'flycheck
  (flycheck-define-checker golangci-lint
    "A Go metalinter, using the golangci-lint v2 command line."
    :command ("golangci-lint" "run"
              "--output.checkstyle.path" "stdout"
              "--show-stats=false"
              "--issues-exit-code" "0"
              "--allow-serial-runners"
              ".")
    :error-parser flycheck-parse-checkstyle
    :modes (go-mode go-ts-mode)
    ;; golangci-lint reads the package from disk, so it must run from the
    ;; file's own directory and only once the buffer matches the file.
    :working-directory (lambda (_checker)
                         (and buffer-file-name
                              (file-name-directory buffer-file-name)))
    :predicate (lambda ()
                 (and buffer-file-name
                      (not (buffer-modified-p)))))

  (add-to-list 'flycheck-checkers 'golangci-lint t))

;; Chain it behind lsp-mode's checker rather than replacing it, so gopls
;; diagnostics stay primary and golangci-lint only adds to them.
;;
;; This cannot go in a `with-eval-after-load' on lsp-diagnostics: that
;; file does not define the `lsp' checker when it loads.  The checker is
;; created lazily by `lsp-diagnostics-lsp-checker-if-needed', called from
;; inside the `lsp-diagnostics-mode' body -- so `flycheck-add-next-checker'
;; would fire against a checker that does not exist yet and signal.
;; `define-minor-mode' runs the mode hook after that body, which is the
;; first moment both checkers are valid.
;;
;; Chaining onto `lsp' globally is fine even though the hook also fires
;; for Python, Rust and Clojure buffers: flycheck honours a next-checker's
;; `:modes', so golangci-lint only ever runs in Go buffers.
(defvar my/golangci-lint-chained nil
  "Non-nil once golangci-lint has been chained behind the `lsp' checker.")

(defun my/chain-golangci-lint ()
  "Append golangci-lint to the `lsp' checker's next-checkers, once."
  (unless my/golangci-lint-chained
    (when (and (fboundp 'flycheck-valid-checker-p)
               (flycheck-valid-checker-p 'lsp)
               (flycheck-valid-checker-p 'golangci-lint))
      (flycheck-add-next-checker 'lsp '(warning . golangci-lint) t)
      (setq my/golangci-lint-chained t))))

(add-hook 'lsp-diagnostics-mode-hook #'my/chain-golangci-lint)

(defun my/go-golangci-lint-project ()
  "Run golangci-lint over the whole module in a compilation buffer.
The live flycheck checker only covers the current package; this is the
equivalent of GoLand's Inspect Code across the project."
  (interactive)
  (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                    (projectile-project-root))
                               default-directory)))
    (compile (concat "golangci-lint run"
                     " --output.text.path stdout"
                     " --output.text.colors=false"
                     " --show-stats=false"
                     " --allow-serial-runners"
                     " ./..."))))

;; --- Code generation: GoLand's Generate menu --------------------------
;;
;; Each of these is a thin wrapper over a binary already installed:
;; gomodifytags, impl and gotests respectively.  Together they cover
;; struct tags, "implement interface" stubs and table-test scaffolding.
;; Bound under `C-c G' in Go buffers, which is free (`C-c g' is magit).
(use-package go-tag
  :ensure t
  :commands (go-tag-add go-tag-remove))

(use-package go-impl
  :ensure t
  :commands (go-impl))

(use-package go-gen-test
  :ensure t
  :commands (go-gen-test-dwim go-gen-test-exported go-gen-test-all))

(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "C-c G t") #'go-tag-add)
  (define-key go-ts-mode-map (kbd "C-c G T") #'go-tag-remove)
  (define-key go-ts-mode-map (kbd "C-c G i") #'go-impl)
  (define-key go-ts-mode-map (kbd "C-c G g") #'go-gen-test-dwim)
  (define-key go-ts-mode-map (kbd "C-c G l") #'my/go-golangci-lint-project))

;; ============================================================
;; 11c. Rust (rust-analyzer)
;; ============================================================

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . lsp)
  ;; `lsp-inlay-hints-mode' deliberately NOT hooked here -- see the note on
  ;; `lsp-inlay-hint-enable' in section 10.  This hook was the original
  ;; source of the bogus "failed to define function lsp-inlay-hints-mode"
  ;; autoload; it just happened to point at rust-ts-mode instead of
  ;; go-ts-mode.  Rust inlay hints now come from the global switch.
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
;; 11i. GitHub Actions workflows
;; ============================================================
;;
;; Two tools, covering different halves of the problem:
;;
;;   gh-actions-language-server -- GitHub's own workflow language
;;     service, the same one behind the VS Code GitHub Actions
;;     extension.  Upstream publishes it as `@actions/languageserver'
;;     with no `bin' entry (actions/languageservices#56), so it cannot
;;     be launched directly; `gh-actions-language-server' is a thin
;;     wrapper package that adds the executable.  Install with:
;;       npm install -g gh-actions-language-server
;;     It answers completion, hover and document links, and pushes
;;     diagnostics.  No formatting and no go-to-definition -- those
;;     capabilities are simply absent from its initialize reply, which
;;     is why it is registered as an add-on below.
;;
;;     What the completion actually covers, measured against this
;;     version: workflow and job and step keys, `on:' event names,
;;     `runs-on:' runner labels, and -- the useful one -- `${{ }}'
;;     expression contexts, including drilling into `github.',
;;     `needs.', `steps.' and friends.  It does NOT complete the
;;     `with:' inputs of a referenced action, even though it does
;;     validate them (a misspelt `fetch-dept:' on actions/checkout is
;;     reported as an error), so that gap is a server limitation, not
;;     a missing token.
;;
;;   actionlint -- static checker that goes well past the schema:
;;     `${{ }}' expression type checking, runner label typos, glob and
;;     cron syntax, and shellcheck run over every `run:' block.
;;       brew install actionlint     (pulls in shellcheck)
;;
;; Both are scoped to `.github/workflows/'.  YAML anywhere else is left
;; alone, which also means a plain YAML buffer still starts no server
;; (yaml-language-server is not installed here).

(defconst my/gh-actions-workflow-regexp
  (rx ".github/workflows/" (+ (not (any "/"))) ".y" (opt "a") "ml" eos)
  "Path pattern matching a GitHub Actions workflow file.")

(defun my/gh-actions-workflow-p (&optional file)
  "Return non-nil when FILE is a GitHub Actions workflow.
FILE defaults to the current buffer's file."
  (let ((name (or file buffer-file-name)))
    (and name (string-match-p my/gh-actions-workflow-regexp name))))

;; --- Language server ---------------------------------------------------
;;
;; A token is what unlocks the second half of this server.  Without one
;; it checks workflows against the bundled schema and nothing more; with
;; one it also fetches the `action.yml' of every action a step
;; references, which is what makes a typo in a `with:' key an error
;; rather than silently ignored input.  Read once from the gh CLI, which
;; already holds a keyring token here.
;;
;; The server takes one further option, `repos', a list of repositories
;; to pull live secret / variable / environment names from.  Not set:
;; supplying it means resolving numeric repo ids up front, and the
;; payoff is only that `${{ secrets. }}' lists more than GITHUB_TOKEN.
;;
;; The token is passed as an initialize option, so it lands in the
;; *lsp-log* buffer if `lsp-log-io' is ever turned on for this server.
(defvar my/gh-actions-session-token 'unset
  "Cached GitHub token for the Actions language server.
The value `unset' means \"not looked up yet\"; nil means \"looked up
and unavailable\", so a missing token is not re-probed on every start.")

(defun my/gh-actions-session-token ()
  "Return a GitHub token for the Actions language server, or nil."
  (when (eq my/gh-actions-session-token 'unset)
    (setq my/gh-actions-session-token
          (let ((token (or (getenv "GITHUB_TOKEN")
                           (when-let* ((gh (executable-find "gh")))
                             (ignore-errors
                               (car (process-lines gh "auth" "token")))))))
            (and (stringp token)
                 (not (string-empty-p (string-trim token)))
                 (string-trim token)))))
  my/gh-actions-session-token)

(defun my/gh-actions-initialization-options ()
  "Build the initialize options for the Actions language server.
Never returns nil.  The server reads `initializationOptions.sessionToken'
without a null guard, so a null object makes it die during initialize --
silently, with no response and no stderr.  Keeping `userAgent' in
unconditionally guarantees a non-empty object even with no token."
  (append (list :userAgent "emacs-lsp-mode")
          (when-let* ((token (my/gh-actions-session-token)))
            (list :sessionToken token))))

(with-eval-after-load 'lsp-mode
  ;; `:add-on? t' rather than a priority: this server handles one narrow
  ;; slice of YAML and provides no formatting, so it should never win the
  ;; language id "yaml" outright.  lsp-mode starts add-on clients
  ;; alongside the main client, and on their own when there is no main
  ;; client -- which is the case today, and stays correct if
  ;; yaml-language-server is installed later.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (list (executable-find "gh-actions-language-server")
                             "--stdio"))
                     (lambda ()
                       (executable-find "gh-actions-language-server")))
    ;; Path-based, not mode-based: `.github/workflows/ci.yml' only.
    :activation-fn (lambda (filename _mode) (my/gh-actions-workflow-p filename))
    :initialization-options #'my/gh-actions-initialization-options
    :add-on? t
    :server-id 'gh-actions-ls)))

(defun my/gh-actions-setup ()
  "Start LSP in GitHub Actions workflow buffers only."
  (when (my/gh-actions-workflow-p)
    (lsp-deferred)))

(add-hook 'yaml-mode-hook #'my/gh-actions-setup)
(add-hook 'yaml-ts-mode-hook #'my/gh-actions-setup)

;; --- actionlint --------------------------------------------------------
;;
;; Flycheck already ships the `yaml-actionlint' checker, gated on the same
;; `.github/workflows' path test used above, so there is nothing to
;; define.  It just has to be chained behind lsp-mode's checker, exactly
;; as golangci-lint is in the Go section -- see the long comment there for
;; why this hangs off `lsp-diagnostics-mode-hook' and not a
;; `with-eval-after-load'.
;;
;; `warning' as the chain level means actionlint runs only once the
;; server itself is happy.  That is deliberate: the two overlap on
;; `with:' input names and would otherwise double-report them.  Fix what
;; the server flags, then actionlint layers on the checks it cannot see
;; -- expression typing and shellcheck over `run:'.  Change the level to
;; a bare `yaml-actionlint' to run both unconditionally instead.
(defvar my/actionlint-chained nil
  "Non-nil once actionlint has been chained behind the `lsp' checker.")

(defun my/chain-actionlint ()
  "Append `yaml-actionlint' to the `lsp' checker's next-checkers, once."
  (unless my/actionlint-chained
    (when (and (fboundp 'flycheck-valid-checker-p)
               (flycheck-valid-checker-p 'lsp)
               (flycheck-valid-checker-p 'yaml-actionlint))
      (flycheck-add-next-checker 'lsp '(warning . yaml-actionlint) t)
      (setq my/actionlint-chained t))))

(add-hook 'lsp-diagnostics-mode-hook #'my/chain-actionlint)

(defun my/gh-actions-lint-project ()
  "Run actionlint over every workflow in the project, in a compilation buffer.
The flycheck checker only sees the current buffer; this is the whole-repo
sweep, and the same thing CI would run."
  (interactive)
  (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                    (projectile-project-root))
                               default-directory)))
    (compile "actionlint -oneline")))

;; `C-c G l' on purpose: the same chord the Go section uses for its
;; whole-project lint sweep, so there is one key to remember.
(with-eval-after-load 'yaml-mode
  (define-key yaml-mode-map (kbd "C-c G l") #'my/gh-actions-lint-project))
(with-eval-after-load 'yaml-ts-mode
  (define-key yaml-ts-mode-map (kbd "C-c G l") #'my/gh-actions-lint-project))

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
