;;; -*- lexical-binding: t; -*-

;; Don't show emojis in these modes
(setq emojify-inhibit-major-modes
      '(dired-mode
        doc-view-mode
        debugger-mode
        pdf-view-mode
        image-mode
        help-mode
        ibuffer-mode
        magit-popup-mode
        magit-diff-mode
        ert-results-mode
        compilation-mode
        proced-mode
        mu4e-headers-mode
        eshell-mode
        term-mode))

;; Emojis everywhere else
(add-hook 'after-init-hook #'global-emojify-mode)
