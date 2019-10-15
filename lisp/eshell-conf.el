;;; -*- lexical-binding: t -*-
;; Nice aliases for find-file
(defalias 'open 'find-file)
(defalias 'openo 'find-file-other-window)
(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)
(defalias 'vi 'find-file)
(defalias 'vim 'find-file)

;; Map over commands that don't work well with eshell
(defun njm-eshell-noop (name &rest rest)
  (message "%s called with: %s" name rest))

;; (eshell/alias "ssh" "njm-eshell-noop ssh $1")
;; (eshell/alias "vi" "find-file $1")
;; (eshell/alias "vim" "find-file $1")

;; use ivy for tab completion
(add-hook 'eshell-mode-hook '(lambda ()
                               (define-key
                                 eshell-mode-map
                                 (kbd "<tab>")
                                 'completion-at-point)))

(setq eshell-history-size 999999)


(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "[" 'face `(:foreground "green"))
         (propertize (format-time-string "%H:%M:%S") 'face `(:foreground "yellow"))
         (propertize "][" 'face `(:foreground "green"))
         (propertize (mehak/pwd) 'face `(:foreground "tan"))
         (propertize "]\n" 'face `(:foreground "green")))))

;; copy and paste these for now :-(
;; for key in ~/.ssh/*.pub { ssh-add $(file-name-sans-extension key) }
;; for key in ~/.ssh/dst/*.pub { ssh-add $(file-name-sans-extension key) }
;; run the above through eshell-parse-command
;;
;;
;; this needs a little work
;; (defun eshell/add-my-keys ()
;;   (interactive)
;;   (dolist (dir '("" "dst")
;;     (let ((glob
;;            (expand-file-name (format "~/.ssh/%s*.pub" dir))))
;;       (dolist (key (eshell-extended-glob glob))
;;         (eshell-trap-errors
;;          (eshell-named-command "ssh-add"
;;                                (list
;;                                 (file-name-sans-extension key))))))))
