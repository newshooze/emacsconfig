; File: init.el
(global-set-key (kbd "<f4>") 'kill-emacs)
(global-set-key (kbd "<S-f4>") '(save-buffers-kill-emacs t))
(global-set-key (kbd "<f8>") 'eval-last-sexp)
(global-set-key (kbd "<S-f8>") 'eval-buffer)
(global-set-key (kbd "<f9>") 'next-buffer)
(global-set-key (kbd "<S-f9>") 'previous-buffer)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default cursor-in-non-selected-windows 'hollow)
(setq-default cursor-type '(hbar . 2))
(setq-default indent-tabs-mode 1)
(setq-default save-place t)
(setq-default tab-width 2)

(setq column-number-mode t)
(setq comint-move-point-for-output t)
(setq compilation-always-kill t)
(setq compilation-scroll-output 1)
(setq completion-ignore-case t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq create-lockfiles nil)
(setq custom-safe-themes t)
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(setq default-truncate-lines t)
(setq font-lock-maximum-decoration t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq	initial-scratch-message ";scratch\n\n")
(setq line-number-mode t)
(setq read-file-name-ignore-completion t)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq tab-stop-list (number-sequence 2 100 2))

(require 'saveplace)
(setq save-place-file (expand-file-name "settings/savedplaces" user-emacs-directory))

(add-to-list 'load-path (concat user-emacs-directory "elpa/company"))
(require 'company)
;company isn't useful in csound-mode
(setq company-global-modes '(not csound-mode not vimhelp-mode))

(add-to-list 'load-path (concat user-emacs-directory "evil"))
(require 'evil)

(add-to-list 'load-path (concat user-emacs-directory "csound"))
(require 'csound-mode)

(add-to-list 'load-path (concat user-emacs-directory "vimhelp"))
(require 'vimhelp-mode)

(add-to-list 'default-frame-alist '(tty-color-mode . 1))

(add-to-list 'load-path (concat user-emacs-directory "themes"))
(load-theme 'csd)

;use vim style scroll up / scroll down
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)

; Start company (complete anything)
(add-hook 'after-init-hook 'global-company-mode)
; Start in vi(m) mode
(evil-mode)
