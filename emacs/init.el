; === PLUGINS ===
; Set up packaging.
(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
  ("melpa" . "http://melpa.org/packages/")
  ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eyebrowse misc protobuf-mode cmake-mode helm intero evil-magit magit evil-search-highlight-persist all-the-icons use-package haskell-mode neotree auto-complete undo-tree ido evil key-chord evil-terminal-cursor-changer company))))
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
(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-wrap-around t)
  (add-hook 'find-file-hook 'eyebrowse-create-window-config))
(require 'misc)

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
; TODO: Make the number dynamic based on the number of lines.
(if (not window-system) (setq linum-format "%5d "))
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
	    (let ((color (cond ((evil-insert-state-p) '("#930000" . "#ffffff"))
			       (t '("#024e8c" . "#ffffff")))))
	      (set-face-background 'mode-line (car color))
	      (set-face-foreground 'mode-line (cdr color)))))

; === BINDINGS ===
; Evaluation.
(global-set-key (kbd "M-e") nil)
(global-set-key (kbd "M-e M-b") 'eval-buffer)
(global-set-key (kbd "M-e M-e") 'eval-expression)
(global-set-key (kbd "M-e M-s") 'eval-last-sexp)
; Window management.
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key (kbd "M-q") 'eyebrowse-close-window-config)
(global-set-key (kbd "M-Q") 'delete-window)
(global-set-key (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
(global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
(global-set-key (kbd "M-w") nil)
(global-set-key (kbd "M-w M-r") 'eyebrowse-rename-window-config)
(global-set-key (kbd "M-w M-c") 'eyebrowse-create-window-config)
(global-set-key (kbd "M-w M-a") 'split-window-horizontally)
(global-set-key (kbd "M-w M-s") 'split-window-vertically)
(global-set-key (kbd "M-w M-q") 'delete-window)
(global-set-key (kbd "<tab>") nil)
(global-set-key [M-tab] 'eyebrowse-next-window-config)
(global-set-key (kbd "<S-tab>") 'eyebrowse-prev-window-config)
(global-set-key (kbd "<M-S-iso-lefttab>") 'eyebrowse-prev-window-config)
(global-set-key (kbd "<backtab>") 'eyebrowse-prev-window-config)
(define-key evil-motion-state-map (kbd "<tab>") nil)
(define-key evil-motion-state-map (kbd "<S-tab>") nil)
(define-key evil-normal-state-map [tab] 'eyebrowse-next-window-config)
(define-key evil-normal-state-map [S-tab] 'eyebrowse-prev-window-config)
; Buffer management.
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
(global-set-key (kbd "M-b") nil)
(global-set-key (kbd "M-b M-q") 'kill-this-buffer)
(global-set-key (kbd "M-b M-p") 'my-previous-buffer)
(global-set-key (kbd "M-b M-n") 'my-next-buffer)
(global-set-key (kbd "M-b M-b") 'buffer-menu)
; Convenient bindings for common operations.
(global-set-key "\M-f" 'ido-find-file)
(global-set-key "\M-u" 'undo-tree-visualize)
(global-set-key "\M-d" 'ido-dired)
; Switch between header and source.
(global-set-key (kbd "<f11>") 'ff-find-other-file)
; Proper forward word behavior with C-Left and C-Right.
(global-set-key [C-right] 'forward-to-word)
; Ctrl-/ to clear search results.
(define-key evil-normal-state-map (kbd "C-/") 'evil-search-highlight-persist-remove-all)
(define-key evil-normal-state-map (kbd "C-_") 'evil-search-highlight-persist-remove-all)
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
; Haskell bindings.
(define-key haskell-mode-map [f3] (lambda () (interactive) (compile "stack build --fast")))
(define-key haskell-mode-map [f12] 'intero-devel-reload)
(define-key haskell-mode-map (kbd "M-h") nil)
(define-key haskell-mode-map (kbd "M-h M-i") 'intero-info)
(define-key haskell-mode-map (kbd "M-h M-t") 'intero-type-at)
(define-key haskell-mode-map (kbd "M-h M-y") 'intero-type-insert)
(define-key haskell-mode-map (kbd "M-h M-l") 'intero-repl-load)
(define-key haskell-mode-map (kbd "M-h M-s") 'intero-apply-suggestions)
(define-key haskell-mode-map (kbd "M-h M-c") 'intero-repl-clear-buffer)
(define-key haskell-mode-map (kbd "M-h M-r") 'intero-repl)
; Increment/decrement numbers.
(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
	(setq inc-by (if arg arg 1))
	(skip-chars-backward "0123456789")
	(when (re-search-forward "[0-9]+" nil t)
	  (setq field-width (- (match-end 0) (match-beginning 0)))
	  (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
	  (when (< answer 0)
	    (setq answer (+ (expt 10 field-width) answer)))
	  (replace-match (format (concat "%0" (int-to-string field-width) "d")
				 answer)))))))
(defun decrement-number-decimal (&optional arg)
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -1)))
(define-key evil-normal-state-map (kbd "C-a") 'increment-number-decimal)
(define-key evil-normal-state-map (kbd "C-x") 'decrement-number-decimal)

; === MISC ===
; Treat underscore as part of a word for C++ and python.
(defun treat-underscore-as-part-of-word ()
  (modify-syntax-entry ?_ "w" (syntax-table)))
(add-hook 'c-mode-hook 'treat-underscore-as-part-of-word)
(add-hook 'c++-mode-hook 'treat-underscore-as-part-of-word)
(add-hook 'python-mode-hook 'treat-underscore-as-part-of-word)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#292929"))))
 '(company-scrollbar-fg ((t (:background "#1c1c1c"))))
 '(company-tooltip ((t (:inherit default :background "#292929"))))
 '(company-tooltip-common ((t (:inherit font-lock-comment-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-keyword-face))))
 '(dired-directory ((t (:inherit font-lock-comment-face)))))
; Smooth scrolling.
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
; Only syntax check on save for Flycheck.
(setq flycheck-check-syntax-automatically '(save))
; Tweak auto-mode alist.
(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.
The return value is the new value of LIST-VAR."
  (set list-var (append (symbol-value list-var) elements)))
(append-to-list 'auto-mode-alist
		'(("\\.bash_aliases" . sh-mode)))
; C++-mode style.
(c-add-style "my-style"
	     '("stroustrup"
	       (indent-tabs-mode . nil) ; use spaces rather than tabs
	       (c-basic-offset . 4))) ; indent by four spaces
(defun my-c++-mode-hook ()
  (c-set-style "my-style") ; use my-style defined above
  (auto-fill-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
; Start the server.
;(server-start)
(message "Finished loading init.el")
