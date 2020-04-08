;;; -*- lexical-binding: t; -*-

;; Grabbed from https://www.masteringemacs.org/article/displaying-interacting-processes-proced
(defun proced-settings ()
  (proced-toggle-auto-update))

;; Uncomment below for auto-updating proced
;; (add-hook 'proced-mode-hook 'proced-settings)
;; (remove-hook 'proced-mode-hook 'proced-settings)
