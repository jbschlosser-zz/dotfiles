; === PLUGINS ===
; Set up packaging.
(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
  ("melpa" . "http://melpa.org/packages/")
  ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(custom-set-variables
 '(package-selected-packages
   (quote
    (helm intero evil-magit magit evil-search-highlight-persist all-the-icons use-package haskell-mode neotree auto-complete undo-tree ido evil key-chord evil-terminal-cursor-changer company))))
(require 'use-package)
(setq use-package-always-ensure t)
(use-package evil :config (evil-mode 1))
(use-package evil-terminal-cursor-changer :if (not window-system)
  :config (evil-terminal-cursor-changer-activate))
(use-package evil-search-highlight-persist
  :config (global-evil-search-highlight-persist t))
(use-package ido
  :init (setq ido-enable-flex-matching t) (setq ido-everywhere t)
  :config (ido-mode 1))
(use-package undo-tree :config (global-undo-tree-mode))
(use-package key-chord :config (key-chord-mode 1))
(use-package company :config (add-hook 'after-init-hook 'global-company-mode))
(use-package all-the-icons)
(use-package magit :bind (("M-g" . magit-status))
  :config
  (setq magit-display-buffer-function
	(lambda (buffer)
	  (display-buffer buffer '(display-buffer-same-window)))))
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))
(use-package intero
  :config
  (defun intero-type-insert ()
    (interactive)
    (intero-type-at t))
  (add-hook 'haskell-mode-hook 'intero-mode)
  (add-hook 'haskell-mode-hook
	    (lambda () (interactive)
	      (define-key evil-motion-state-map "gd" 'intero-goto-definition))))
(use-package color)

; === VISUALS ===
; Load theme.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sunburst t)
; Turn off unnecessary UI elements.
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
; Disable cursor blinking.
(blink-cursor-mode 0)
; Disable startup screen.
(setq inhibit-startup-screen t)
; Set custom scratch message.
(setq initial-scratch-message "; *scratch*\n")
; Turn on line numbers.
(global-linum-mode 1)
; Set line number color.
(set-face-foreground 'linum "#bf8900")
; Set font.
(setq my-default-font "Terminus 12")
(set-face-attribute 'default t :font my-default-font)
(set-face-attribute 'default nil :font my-default-font)
(set-frame-font my-default-font nil t)
; Mode-based cursor box colors (DISABLED FOR NOW DUE TO INTERO BUG).
(setq evil-normal-state-cursor '(box "orange"))
(setq evil-insert-state-cursor '(box "orange"))
; Show matching parens.
(setq show-paren-delay 0)
(show-paren-mode 1)
; Set matching paren color.
(set-face-background 'show-paren-match "white")
(set-face-foreground 'show-paren-match "black")
(set-face-attribute 'show-paren-match nil :weight 'bold)
; Change mode-line color by evil state.
(add-hook 'post-command-hook
	  (lambda ()
	    (let ((color (cond ((evil-insert-state-p) '("#f45942" . "#ffffff"))
			       (t '("#024e8c" . "#ffffff")))))
	      (set-face-background 'mode-line (car color))
	      (set-face-foreground 'mode-line (cdr color)))))

; === BINDINGS ===
; Window movement.
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
; Convenient bindings for common operations.
(global-set-key "\M-f" 'ido-find-file)
(global-set-key "\M-u" 'undo-tree-visualize)
(global-set-key "\M-q" 'kill-this-buffer)
(global-set-key "\M-Q" 'delete-window)
(global-set-key "\M-b" 'buffer-menu)
(global-set-key "\M-d" 'ido-dired)
; Evaluation.
(global-set-key "\M-e" nil)
(global-set-key (kbd "M-e b") 'eval-buffer)
(global-set-key (kbd "M-e e") 'eval-expression)
(global-set-key (kbd "M-e s") 'eval-last-sexp)
; Switch between header and source.
(global-set-key (kbd "<f11>") 'ff-find-other-file)
; Ctrl-/ to clear search results.
(define-key evil-normal-state-map (kbd "C-/") 'evil-search-highlight-persist-remove-all)
; Enable Ctrl-W inside the minibuffer.
(define-key minibuffer-local-map "\C-w" 'backward-kill-word)
; Enable Ctrl-U to delete to beginning of line.
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key "\C-u" 'backward-kill-line)
; Spacebar scrolls cursor line to the center of the window.
(define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-line-to-center)
; Buffer switching with M-Tab and M-S-Tab.
(defun my-next-buffer ()
  "next-buffer, skip *name* buffers except *scratch*"
  (interactive)
  (next-buffer)
  (while (and (string-prefix-p "*" (buffer-name))
	      (not (string= "*scratch*" (buffer-name))))
      (next-buffer)))
(defun my-previous-buffer ()
  "previous-buffer, skip *name* buffers except *scratch*"
  (interactive)
  (previous-buffer)
  (while (and (string-prefix-p "*" (buffer-name))
	      (not (string= "*scratch*" (buffer-name))))
      (previous-buffer)))
(global-set-key [M-tab] 'my-previous-buffer)
(global-set-key (kbd "<M-S-iso-lefttab>") 'my-next-buffer)
(global-set-key (kbd "<backtab>") 'my-next-buffer)
; Setup ESC to quit most things.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
; Proper tab complete.
(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete)
    (indent-according-to-mode)))
(define-key evil-insert-state-map [tab] 'complete-or-indent)
(define-key evil-normal-state-map [tab] 'indent-according-to-mode)
; Haskell bindings.
(define-key haskell-mode-map [f3] (lambda () (interactive) (compile "stack build --fast")))
(define-key haskell-mode-map [f12] 'intero-devel-reload)
(define-key haskell-mode-map (kbd "M-h") nil)
(define-key haskell-mode-map (kbd "M-h i") 'intero-info)
(define-key haskell-mode-map (kbd "M-h t") 'intero-type-at)
(define-key haskell-mode-map (kbd "M-h y") 'intero-type-insert)
(define-key haskell-mode-map (kbd "M-h l") 'intero-repl-load)
(define-key haskell-mode-map (kbd "M-h s") 'intero-apply-suggestions)
(define-key haskell-mode-map (kbd "M-h c") 'intero-repl-clear-buffer)
(define-key haskell-mode-map (kbd "M-h r") 'intero-repl)

; === MISC ===
; Set middle-click to paste at cursor position instead of mouse position.
(setq mouse-yank-at-point t)
; Turn off line wrapping by default.
(set-default 'truncate-lines t)
; Don't go past the end of the buffer for cursor movements (e.g. G in evil).
(defun end-of-buffer-dwim (&rest args)
  (when (looking-at-p "^$")
    (previous-line))
  (beginning-of-line))
(advice-add 'end-of-buffer :after 'end-of-buffer-dwim)
; Store all backup and autosave files in the tmp dir.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
; Don't auto-insert a final newline.
(setq require-final-newline nil)
; Set some custom stuff.
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   '(dired-directory ((t (:inherit font-lock-comment-face))))
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-keyword-face))))
   `(company-tooltip-common ((t (:inherit font-lock-comment-face))))))
; Only syntax check on save for Flycheck.
(setq flycheck-check-syntax-automatically '(save))

(message "Finished loading init.el")
