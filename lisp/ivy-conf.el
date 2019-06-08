;;; -*- lexical-binding: t; -*-

;; Counsel + flx (swiper ivy) ;;
;; Ivy Mode
(ivy-mode 1)
;; Use fuzzy search after every character
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)

;; basic customization
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
;; use arrow for current selection
(setq ivy-format-function #'ivy-format-function-line)

;; magit ivy-completion
(setq magit-completing-read-function 'ivy-completing-read)

;; ivy-rich
(require 'ivy-rich)
(ivy-rich-mode 1)

;; Rebind M-x to counsel-M-x
(global-set-key "\M-x" 'counsel-M-x)
;; Rebind C-h f to counsel-describe-function
(global-set-key "\C-hf" 'counsel-describe-function)
;; Rebind C-h v to counsel-describe-variable
(global-set-key "\C-hv" 'counsel-describe-variable)
