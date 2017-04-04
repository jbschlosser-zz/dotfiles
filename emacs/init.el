; Set up packaging.
(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
  ("melpa" . "http://melpa.org/packages/")
  ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
; Load theme.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sunburst t)
; Load evil mode.
(require 'evil)
(evil-mode 1)
; Setup ido.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))
; Load some other plugins.
(key-chord-mode 1)
(global-undo-tree-mode)
; Turn on line numbers.
(global-linum-mode 1)
(set-face-foreground 'linum "#bf8900")
; Set some custom stuff.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (sunburst)))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(package-selected-packages
   (quote
    (ac-haskell-process haskell-mode neotree auto-complete undo-tree ido evil key-chord evil-terminal-cursor-changer evil-tabs company)))
 '(require-final-newline nil)
 '(tool-bar-mode nil))
; Set font.
(setq my-default-font "Terminus 12")
(set-face-attribute 'default t :font my-default-font)
(set-face-attribute 'default nil :font my-default-font)
(set-frame-font my-default-font nil t)
; Enable middle-click pasting at cursor position.
(setq mouse-yank-at-point t)
; Enable Ctrl-U to delete to beginning of line.
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key "\C-u" 'backward-kill-line)
; Mode-based cursor box colors.
(setq evil-normal-state-cursor '(box "orange"))
(setq evil-insert-state-cursor '(box "yellow"))
; Cycle through tabs.
(define-key evil-normal-state-map (kbd "<tab>")
  (lambda () (interactive) (tabbar-forward)))
(define-key evil-normal-state-map (kbd "<backtab>")
  (lambda () (interactive) (tabbar-backward)))
(define-key evil-normal-state-map (kbd "<S-tab>")
  (lambda () (interactive) (tabbar-backward)))
; Disable mouse cursor movement.
;; (defun silence () (interactive))
;; (define-key evil-motion-state-map [down-mouse-1] 'silence)
;; (define-key evil-motion-state-map [down-mouse-2] 'silence)
;; (define-key evil-motion-state-map [mouse-1] 'silence)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key "\M-f" 'ido-find-file)
(evil-define-command evil-scroll-line-to-center (count)
  "Scrolls line number COUNT (or the cursor line) to the center of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter nil)))
(define-key evil-normal-state-map (kbd "SPC")
  'evil-scroll-line-to-center)
(define-key minibuffer-local-map "\C-w" 'backward-kill-word)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(require 'ac-haskell-process)
(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-interactive-mode))
; Set proper cursor movement.
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
; Kill current buffer.
(global-set-key "\M-q" 'kill-this-buffer)
; Buffer switching.
(defun my-next-buffer ()
  "next-buffer, skip *Messages* and *Help*"
  (interactive)
  (next-buffer)
  (when (or (string= "*Messages*" (buffer-name))
	    (string= "*Help*" (buffer-name)))
      (next-buffer)))
(defun my-previous-buffer ()
  "previous-buffer, skip *Messages* and *Help*"
  (interactive)
  (previous-buffer)
  (when (or (string= "*Messages*" (buffer-name))
	    (string= "*Help*" (buffer-name)))
      (previous-buffer)))
(global-set-key [M-tab] 'my-next-buffer)
(global-set-key (kbd "<M-S-iso-lefttab>") 'my-previous-buffer)
; Disable startup screen.
(setq inhibit-startup-screen t)
(message "Finished loading init.el")
