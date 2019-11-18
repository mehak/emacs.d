;;; -*- lexical-binding: t; -*-

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

(setq term-suppress-hard-newline t)
