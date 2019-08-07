;;; -*- lexical-binding: t; -*-

;; Counsel + flx (swiper ivy) ;;
;; Ivy Mode
(ivy-mode 1)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
(setq ivy-initial-inputs-alist
      '((org-refile . "^")
        (org-agenda-refile . "^")
        (org-capture-refile . "^")
        (counsel-M-x . "^")
        (counsel-describe-function . "^")
        (counsel-describe-variable . "^")
        (counsel-org-capture . "^")
        (Man-completion-table . "^")
        (woman . "^")))

;; basic customization
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
;; use arrow for current selection
(setq ivy-format-function #'ivy-format-function-line)

;; magit ivy-completion
(setq magit-completing-read-function 'ivy-completing-read)

;; ivy-rich
(require 'ivy-rich)

;; Rebind M-x to counsel-M-x
(global-set-key "\M-x" 'counsel-M-x)
;; Rebind C-h f to counsel-describe-function
(global-set-key "\C-hf" 'counsel-describe-function)
;; Rebind C-h v to counsel-describe-variable
(global-set-key "\C-hv" 'counsel-describe-variable)

;; Show EXWM class or major mode
(defun mehak/ivy-rich-switch-buffer-major-mode (candidate)
  (with-current-buffer
      (get-buffer candidate)
    (mehak/major-mode-or-class)))

(setq ivy-rich-display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-candidate (:width 50))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (mehak/ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
        counsel-M-x
        (:columns
         ((counsel-M-x-transformer (:width 40))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-function
        (:columns
         ((counsel-describe-function-transformer (:width 40))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-variable
        (:columns
         ((counsel-describe-variable-transformer (:width 40))
          (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
        counsel-recentf
        (:columns
         ((ivy-rich-candidate (:width 0.8))
          (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))

;; Enable ivy-rich mode after defining column transformers
(ivy-rich-mode 1)
