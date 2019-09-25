;;; -*- lexical-binding: t; -*-

;; Setup evil, evil-leader, and evil-collection
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  (evil-set-initial-state 'exwm-mode 'emacs))

(use-package evil-leader
  :ensure t
  :after evil-collection
  :init
  :config)

;; evil-magit
(require 'evil-magit)

;; evil-lisp
(require 'evil-lispy)
(add-hook 'lisp-mode-hook #'evil-lispy-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
(setq lispy-use-sly t)

;; Some evil-ex commands
(evil-ex-define-cmd "bd[elete]" 'kill-buffer)
