;;; -*- lexical-binding: t; -*-

;;; Emacs base keys
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-x" 'counsel-M-x)
(global-set-key "\C-hf" 'counsel-describe-function)
(global-set-key "\C-hv" 'counsel-describe-variable)


;;; SDCV keys
(global-set-key (kbd "<f11>") 'sdcv-search-input)
(global-set-key (kbd "<f10>") 'sdcv-search-pointer)
;; This is a hack so that the sdvc buffer/frame/window close together
(evil-define-key 'normal sdcv-mode-map
  "q" 'quit-window)


;;; evil
(define-key evil-motion-state-map (kbd "SPC") nil)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "xf" 'find-file
  "xb" 'ivy-switch-buffer
  "xg" 'magit-status
  ";" 'eval-expression
  "d" 'kill-whole-line
  "h?" 'help-for-help
  "hw" 'where-is
  "hf" 'counsel-describe-function
  "hk" 'describe-key)


;;; Misc task bindings
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)
