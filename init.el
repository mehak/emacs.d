;;; -*- lexical-binding: t -*-


;; Had these originally but ripped off the if (fboundp bits from
;; https://github.com/penryu/emacs.d
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; TODO re-write to use cl-loop or dolist
(defun ensure-package-installed (&rest packages)
  "Assure everypackage is installed, ask for installing if it's not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))


;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(package-initialize)

(ensure-package-installed 'evil
                          'slack
                          'magit
                          'evil-magit
                          'use-package
                          'counsel
                          'yasnippet)

(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages '(all))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" default)))
 '(org-export-backends (quote (ascii html icalendar latex man md odt org texinfo)))
 '(package-selected-packages
   (quote
    (ivy-rich command-log-mode ivy-hydra evil-collection company sly exwm cyberpunk-theme counsel evil-lispy rainbow-delimiters evil-magit ox-reveal ## auto-org-md kotlin-mode xpm yasnippet-snippets yasnippet csharp-mode coffee-mode lua-mode xml-rpc use-package spinner slack queue powershell pkg-info org magit highlight-symbol helm evil-visual-mark-mode csv-mode))))

(set-face-attribute 'default nil :font "Source Code Pro" :height 83)


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
  (evil-collection-init))


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


;;;;;;;;;;; Indentation ;;;;;;;;;;;;;;;;;;;;

;; Set javascript to use 4 spaces on indent
(setq js-indent-level 4)


;; Powershell modes
;; need to fix this for pwsh
(setq powershell-indent 2)
(setq powershell-location-of-exe "/usr/bin/pwsh")


;;;;;;;;;;; Indentation ;;;;;;;;;;;;;;;;;;;;


;; Setup a ruler (vim) like display at bottom
(line-number-mode 1)
(column-number-mode 1)

;; Don't show welcome screen
(setq inhibit-startup-screen t)

;; Enable erase-buffer
(put 'erase-buffer 'disabled nil)

;; Copy to primary clipboard too
(setq x-select-enable-primary t)

(fset 'perl-mode 'cperl-mode)


;; Term mode and helpful keybindings
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


(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify)
  (setq alert-fade-time 30)
  (setq alert-persist-idle-time 1))

(global-unset-key (kbd "C-z"))


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


;; whitespace-mode
(require 'whitespace)
;; automatically clean up bad whitespace
(setq whitespace-action '(auto-cleanup))
;; only show bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))


;; yassnippet
(yas-global-mode 1)




;; Org-mode reveal.js export
(require 'ox-reveal)
(setq org-reveal-title-slide  "<p>%t</p><p>%a</p>")


;; Smartparens minor mode
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)


;; evil-magit
(require 'evil-magit)


;; rainbow delimeters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; evil-lisp
(require 'evil-lispy)
(add-hook 'lisp-mode-hook #'evil-lispy-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)


;; Some evil-ex commands
(evil-ex-define-cmd "bd[elete]" 'kill-buffer)


;; Company mode
(add-hook 'after-init-hook 'global-company-mode)


;; pdf-tools
(pdf-tools-install)


;; erc
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)


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


(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/vendor/")


;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
         auto-mode-alist)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; need to fix this so I don't have to specify particular files
;; and/or lazy load stuff
(load "~/.emacs.d/lisp/exwm-conf.el")
(load "~/.emacs.d/lisp/eshell-conf.el")
(load "~/.emacs.d/lisp/custom.el")
(load "~/.emacs.d/lisp/nogit/slack.el")
(eshell)
