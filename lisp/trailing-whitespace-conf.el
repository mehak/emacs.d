;;; -*- lexical-binding: t; -*-

;; Show trailing whitespace and remove it easily
(setq-default show-trailing-whitespace t)

;; https://emacs.stackexchange.com/a/40649
(defun hide-trailing-whitespace-for-modes ()
  "Disable `show-trailing-whitespace' in selected modes."
  (when (or (derived-mode-p
             'shell-mode
             'erc-mode
             'eshell-mode
             'term-mode)
            (minibufferp))
    (setq show-trailing-whitespace nil)))

(add-hook 'after-change-major-mode-hook
          'hide-trailing-whitespace-for-modes)
