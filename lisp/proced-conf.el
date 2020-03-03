;;; -*- lexical-binding: t; -*-

;; Grabbed from https://www.masteringemacs.org/article/displaying-interacting-processes-proced
(defun proced-settings ()
  (proced-toggle-auto-update))

(add-hook 'proced-mode-hook 'proced-settings)
