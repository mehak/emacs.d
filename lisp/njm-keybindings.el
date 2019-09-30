;;; -*- lexical-binding: t; -*-

;;; Emacs base keys
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-x" 'counsel-M-x)
(global-set-key "\C-hf" 'counsel-describe-function)
(global-set-key "\C-hv" 'counsel-describe-variable)


;;; SDCV keys
(global-set-key (kbd "<f11>") 'sdcv-search-input)
(global-set-key (kbd "<f10>") 'sdcv-search-pointer)
;; This is a hack so that the sdvc buffer/frame/window close together
(evil-define-key 'normal sdcv-mode-map
  "q" 'quit-window)


;;; evil
(define-key evil-motion-state-map (kbd "SPC") nil)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "xf" 'find-file
  "xb" 'ivy-switch-buffer
  "xg" 'magit-status
  ";" 'eval-expression
  "d" 'kill-whole-line
  "h?" 'help-for-help
  "hw" 'where-is
  "hf" 'counsel-describe-function
  "hk" 'describe-key)


;;; Misc task bindings
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)


;; EXWM
;; I use the commands because setting exwm-input-(global|simulation)-keys
;; doesn't seem to work properly (or immediately)
;; Line-editing shortcuts for simulation keys
(exwm-input-set-simulation-key [?\C-b] [left])
(exwm-input-set-simulation-key [?\C-f] [right])
(exwm-input-set-simulation-key [?\C-p] [up])
(exwm-input-set-simulation-key [?\C-n] [down])
(exwm-input-set-simulation-key [?\C-a] [home])
(exwm-input-set-simulation-key [?\C-e] [end])
(exwm-input-set-simulation-key [?\M-v] [prior])
(exwm-input-set-simulation-key [?\C-v] [next])
(exwm-input-set-simulation-key [?\C-d] [delete])
(exwm-input-set-simulation-key [?\C-k] [S-end delete])

;; Global keys
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-s") #'mehak/screenshot)
(exwm-input-set-key (kbd "<print>") #'mehak/screenshot)
;; 's-&': Launch application
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
;; Toggle line-mode/char-mode
(exwm-input-set-key (kbd "s-i")
                    (lambda () (interactive)
                      (exwm-input-toggle-mode)))
;; Lock screen
(exwm-input-set-key (kbd "s-l")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "/usr/bin/slock" nil "/usr/bin/slock")))
;; 's-N': Switch to certain workspace
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
