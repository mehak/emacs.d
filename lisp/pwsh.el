;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This *DOES NOT WORK*
;; You are better off just running comint-run /usr/bin/pwsh
;; (let ((buffer (comint-check-proc "pwsh")))
;; (make-comint-in-buffer "pwsh" buffer
;;                         pwsh-file-path)
;; (pop-to-buffer-same-window "*pwsh*"))

;; (comint-run "/usr/bin/pwsh")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pwsh-file-path "/usr/bin/pwsh"
  "Path to pwsh")

(defvar pwsh-cli-arguments '()
  "Commandline arguments to pass to pwsh")

(defvar pwsh-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    ;; (define-key map "\t" 'completion-at-point)
    map)
  "Basice mode map for pwsh")

(defvar pwsh-prompt-regexp "^PS .*?> "
  "Prompt for pwsh")

(defun pwsh ()
  "Run an inferior pwsh"
  (interactive)
  (let* ((pwsh-program pwsh-file-path)
         (buffer (comint-check-proc "pwsh")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'pwsh-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*pwsh*"))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer "pwsh" buffer
             pwsh-program pwsh-cli-arguments)
      ;; (pwsh-mode)
      t)))

(defun pwsh--initialize ()
  "Helper function to initialize pwsh"
  (setq comint-process-echoes t
        comint-use-prompt-regex t))

(define-derived-mode pwsh-mode comint-mode "pwsh"
  "Major mode for pwsh"
  nil "pwsh"
  ;; (setq comint-prompt-regexp pwsh-prompt-regexp
  ;;       comint-prompt-read-only t)

  ;; (defconst pwsh-keywords
  ;;   '(""))

  ;; (defvar pwsh-font-lock-keywords
  ;;   (list
  ;;    `(,(concat "\\_<" (regexp-opt pwsh-keywords) "\\_>" . font-lock-keyword-face)))
  ;;   "Additional expression to highlight in pwsh-mode.")

  ;; (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) '(pwsh-font-lock-keywords t))
  ;; (set (make-local-variable 'paragraph-start) pwsh-prompt-regexp)
  )

(add-hook 'pwsh-mode-hook 'pwsh--initialize)
;; (remove-hook 'pwsh-mode-hook 'pwsh--initialize)


