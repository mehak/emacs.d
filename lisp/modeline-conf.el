;;; -*- lexical-binding: t; -*-

;; Setup a ruler (vim) like display at bottom
(line-number-mode 1)
(column-number-mode 1)

;; Minions - minor mode menus
(use-package minions
  :config
  (minions-mode 1))

;; Battery mode
(display-battery-mode 1)

;; Show the time
(setq display-time-string-forms
      '((format-time-string " [%F %R] ")))
(display-time-mode 1)

(setq mehak-mode-line-position
      '(line-number-mode
        ("%l"
         (column-number-mode " %C"))))

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mehak-mode-line-position
   " "
   mode-line-modified
   " "
   evil-mode-line-tag
   minions-mode-line-modes
   mode-line-misc-info
   mode-line-buffer-identification
   mode-line-end-spaces))
