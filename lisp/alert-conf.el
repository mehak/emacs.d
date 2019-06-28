;;; -*- lexical-binding: t; -*-

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify)
  (setq alert-fade-time 30)
  (setq alert-persist-idle-time 1))

(global-unset-key (kbd "C-z"))
