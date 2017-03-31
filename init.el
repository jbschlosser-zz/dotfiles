; Set up packaging.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
; Load evil mode.
(add-to-list 'load-path "~/.emacs.d/pkg/evil")
(require 'evil)
(evil-mode 1)
(require 'evil-tabs)
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))
(global-evil-tabs-mode t)
; Load some other plugins.
(require 'company)
(require 'key-chord)
(key-chord-mode 1)
(require 'ido)
(ido-mode t)
(require 'undo-tree)
(global-undo-tree-mode)
; Turn on line numbers.
(global-linum-mode 1)
; Set some custom stuff.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ; Turn off toolbar.
 '(tool-bar-mode nil)
 ; Turn off menubar.
 '(menu-bar-mode nil)
 ; Turn off cursor blinking.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (tango-dark))))
; Fix block cursor in terminal mode?
(setq visible-cursor nil)
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
  (lambda () (interactive) (elscreen-next)))
(define-key evil-normal-state-map (kbd "<backtab>")
  (lambda () (interactive) (elscreen-previous)))
(define-key evil-normal-state-map (kbd "<S-tab>")
  (lambda () (interactive) (elscreen-previous)))
; Disable mouse cursor movement.
(defun silence () (interactive))
;(define-key evil-motion-state-map [down-mouse-1] 'silence)
;(define-key evil-motion-state-map [down-mouse-2] 'silence)
;(define-key evil-motion-state-map [mouse-1] 'silence)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key "\M-f" 'ido-find-file)
(global-set-key "\M-q" 'kill-this-buffer)
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
