; === PLUGINS ===
; Set up packaging.
(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
  ("melpa" . "http://melpa.org/packages/")
  ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(setq use-package-always-ensure t)
(use-package evil :config (evil-mode 1))
  ;(evil-select-search-module 'evil-search-module 'isearch))
(use-package evil-terminal-cursor-changer :if window-system
  :config (evil-terminal-cursor-changer-activate))
(use-package evil-search-highlight-persist
  :config (global-evil-search-highlight-persist t))
(use-package ido
  :init (setq ido-enable-flex-matching t) (setq ido-everywhere t)
  :config (ido-mode 1))
(use-package undo-tree :config (global-undo-tree-mode))
(use-package key-chord :config (key-chord-mode 1))
(use-package ac-haskell-process
  :config
  (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
  (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'haskell-interactive-mode)))
(use-package all-the-icons)

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
; Mode-based cursor box colors.
(setq evil-normal-state-cursor '(box "orange"))
(setq evil-insert-state-cursor '(box "yellow"))
; Show matching parens.
(setq show-paren-delay 0)
(show-paren-mode 1)
; Set matching paren color.
(set-face-background 'show-paren-match "white")
(set-face-foreground 'show-paren-match "black")
(set-face-attribute 'show-paren-match nil :weight 'bold)

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
(global-set-key "\M-a" 'buffer-menu)
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
(global-set-key [M-tab] 'my-next-buffer)
(global-set-key (kbd "<M-S-iso-lefttab>") 'my-previous-buffer)
(global-set-key (kbd "<backtab>") 'my-previous-buffer)

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
; Enable autocomplete.
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
; Don't auto-insert a final newline.
(setq require-final-newline nil)
; Set some custom stuff.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-search-highlight-persist all-the-icons use-package ac-haskell-process haskell-mode neotree auto-complete undo-tree ido evil key-chord evil-terminal-cursor-changer company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "Finished loading init.el")
