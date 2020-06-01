;;; -*- lexical-binding: t; -*-

;;; Emacs base keys
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-x" 'counsel-M-x)
(global-set-key "\C-hf" 'counsel-describe-function)
(global-set-key "\C-hv" 'counsel-describe-variable)

;;; Misc task bindings
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)

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
  "hk" 'describe-key
  "c" 'calendar
  "ws" 'window-configuration-to-register
  "wr" 'jump-to-register
  "xr" 'njm/exwm/refresh)


;; EXWM
;; I use the commands because setting exwm-input-(global|simulation)-keys
;; doesn't seem to work properly (or immediately)
;; Line-editing shortcuts for simulation keys
(dolist (key-spec
         '(([?\C-b] . [left])
           ([?\C-f] . [right])
           ([?\C-p] . [up])
           ([?\C-n] . [down])
           ([?\C-a] . [home])
           ([?\C-e] . [end])
           ([?\M-v] . [prior])
           ([?\C-v] . [next])
           ([?\C-d] . [delete])
           ([?\C-k] . [S-end delete])))
  (exwm-input-set-simulation-key (car key-spec) (cdr key-spec)))

;; Global keys
(dolist (key-spec
         '(("s-r" . exwm-reset)
           ("s-w" . exwm-workspace-switch)
           ("s-f" . counsel-linux-app)
           ("s-s" . mehak/screenshot)
           ("<print>" . mehak/screenshot)
           ("s-l" . mehak/lock-computer)))
  (exwm-input-set-key (kbd (car key-spec)) (cdr key-spec)))
;; 's-&': Launch application
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
;; Toggle line-mode/char-mode
(exwm-input-set-key (kbd "s-i")
                    (lambda () (interactive)
                      (exwm-input-toggle-mode)))
;; 's-N': Switch to certain workspace
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
