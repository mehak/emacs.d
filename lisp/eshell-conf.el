;; Nice aliases for find-file
(defalias 'open 'find-file)
(defalias 'openo 'find-file-other-window)
(defalias 'ff 'find-file)
(defalias 'vi 'find-file)
(defalias 'vim 'find-file)
(defalias 'ffo 'find-file-other-window)

;; use ivy for tab completion
(add-hook 'eshell-mode-hook '(lambda ()
                               (define-key
                                 eshell-mode-map
                                 (kbd "<tab>")
                                 'completion-at-point)))

(setq eshell-history-size 999999)

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
