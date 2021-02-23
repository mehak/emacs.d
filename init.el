;;; -*- lexical-binding: t -*-


;; Had these originally but ripped off the if (fboundp bits from
;; https://github.com/penryu/emacs.d
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove custom if possible
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; pulled out of custom.el
(setq blink-cursor-mode nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(package-initialize)

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages '(all))

(set-face-attribute 'default nil :font "Source Code Pro" :height 83)



;;;;;;;;;;;;;;;;;;;;;;;; Indentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set javascript to use 4 spaces on indent
(setq js-indent-level 4)
;; Powershell modes
(setq powershell-indent 2)
(setq powershell-location-of-exe "/usr/bin/pwsh")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; Theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Don't show welcome screen
(setq inhibit-startup-screen t)

;; Enable erase-buffer
(put 'erase-buffer 'disabled nil)

;; Copy to primary clipboard too
(setq x-select-enable-primary t)

(fset 'perl-mode 'cperl-mode)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 75)

;; Save mini-buffer
(savehist-mode 1)

;; Don't paste at mouse position
(setq mouse-yank-at-point t)


;; Smart Tab
(require 'smart-tab)
(global-smart-tab-mode 1)


;; Sly
(setq inferior-lisp-program "/usr/bin/sbcl")


;; yassnippet
(yas-global-mode 1)


;; Org-mode reveal.js export
(require 'ox-reveal)
(setq org-reveal-title-slide  "<p>%t</p><p>%a</p>")


;; Smartparens minor mode
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)


;; rainbow delimeters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;; Company mode
(setq company-idle-delay 1
      company-quickhelp-color-foreground "#DCDCCC"
      company-quickhelp-color-background "#4F4F4F")
(add-hook 'after-init-hook 'global-company-mode)


;; pdf-tools
(pdf-loader-install)


;; tramp
(setq tramp-default-method "ssh")


;; Use lexical binding by default
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (let  ((auto-insert-query nil)
                   (auto-insert-alist
                    '((("\\.el\\'" . "Emacs Lisp header")
                       ""
                       ";;; -*- lexical-binding: t; -*-\n\n" '(setq lexical-binding t)))))
              (auto-insert))))


;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
      auto-mode-alist)


;; Set eww line width
(setq shr-width 75)

;; Move tooltips to the echo line
(tooltip-mode -1)
(setq tooltip-mode nil)


;; need to fix this so I don't have to specify particular files
;; and/or lazy load stuff
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/vendor/")
(load "misc-functions")
(load "trailing-whitespace-conf")
(load "evil-conf")
(load "alert-conf")
(load "exwm-conf")
(load "eshell-conf")
(load "ibuffer-conf")
(load "modeline-conf")
(load "cyphejor-conf")
(load "ivy-conf")
(load "erc-conf")
(load "magit-conf")
(load "term-conf")
(load "sdcv-conf")
(load "emojify-conf")
(load "documentation")
(load "dired-conf")
(load "tiny-conf")
(load "proced-conf")
(load "purescript-conf")
(load "vterm-conf")
(require 'site-up)
;; Make sure to load this last
;; TODO move to general.el
(load "njm-keybindings")


(eshell)
