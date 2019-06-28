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


;;;;;;;;;;;;;;;;;;;;;;;; Term-Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'term)

;; Helpful keybindings for term-mode
;; http://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term/
(defun jnm/term-toggle-mode ()
  "Toggle between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-k") 'jnm/term-toggle-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; Alerts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify)
  (setq alert-fade-time 30)
  (setq alert-persist-idle-time 1))

(global-unset-key (kbd "C-z"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; Encryption ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy PG (GPG)
(require 'epa-file)
(epa-file-enable)


(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value
  nil
  ;; The indicator for the mode line
  " Sensitive"
  ;; The minor mode bindings
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
    ;; resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))

(setq auto-mode-alist
      (append '(("\\.gpg$" . sensitive-mode))
              auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;; Magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show differences for specific hunk only
(setq-default magit-diff-refine-hunk t)
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

;; Make ibuffer default
(global-set-key "\C-x\C-b" 'ibuffer)

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


;;;;;;;;;;;;;;;;;;;;;;;; erc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erc
(setq
 erc-log-channels-directory "~/.erc/logs/"
 erc-save-buffer-on-part nil
 erc-save-queries-on-quit nil
 erc-log-write-after-send t
 erc-log-write-after-insert t
 erc-rename-buffers t
 erc-nick "mehak"
 erc-server "i.b1n.win"
 erc-port 6697
 erc-fill-column 80)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;; Shut off pronounce for sdcv
(require 'sdcv)
(setq sdcv-word-pronounce nil)
(global-set-key (kbd "<f11>") 'sdcv-search-input)
(global-set-key (kbd "<f10>") 'sdcv-search-pointer)


;; need to fix this so I don't have to specify particular files
;; and/or lazy load stuff
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/vendor/")
(load "misc-functions")
(load "exwm-conf")
(load "eshell-conf")
(load "ibuffer-conf")
(load "modeline-conf")
(load "cyphejor-conf")
(load "ivy-conf")


;;;;;;;;;;;;;;;;;;;;;;;; Emojify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NO EMOJIS HERE!!!!
(setq emojify-inhibit-major-modes '(dired-mode doc-view-mode debugger-mode pdf-view-mode image-mode help-mode ibuffer-mode magit-popup-mode magit-diff-mode ert-results-mode compilation-mode proced-mode mu4e-headers-mode eshell-mode term-mode))
;; Emojis everywhere else
(add-hook 'after-init-hook #'global-emojify-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eshell)
