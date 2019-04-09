;;; -*- lexical-binding: t -*-
;; EXWM configuration
;; Allow fucus to follow mouse
;; must be before ('require exwm)
(setq mouse-autoselect-window 1
      focus-follows-mouse 1)
(require 'exwm)
(require 'exwm-config)
;; the below is basically (exwm-config-default) minus ido
;; Set the initial workspace number.
(setq exwm-workspace-number 2)

;; Make class + title for buffer name
;; Extra space due to bug in swiper, possibly?
(defun mehak/exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer (concat exwm-title " ")))

;; Set hooks
(add-hook 'exwm-update-class-hook 'mehak/exwm-update-buffer-name)
(add-hook 'exwm-update-title-hook 'mehak/exwm-update-buffer-name)


;; 's-r': Reset
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; 's-w': Switch workspace
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; 's-N': Switch to certain workspace
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
;; 's-&': Launch application
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))
;; Line-editing shortcuts
(setq exwm-input-simulation-keys
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
;; show all buffers and allow moving windows from inactive workspace
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

;; Toggle line/char mode
;; https://www.reddit.com/r/emacs/comments/7jftjw/exwm_evil_mode/dr6imc6
(defun exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (second (second mode-line-process)) "line")
          (exwm-input-char-mode)
        (exwm-input-line-mode)))))

(exwm-input-set-key (kbd "s-i")
                    (lambda () (interactive)
                      (exwm-input-toggle-mode)))
(exwm-input-set-key (kbd "s-l")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command "/usr/bin/slock" nil "/usr/bin/slock")))

;; Enable EXWM
(exwm-enable)

;; exwm-randr
(require 'exwm-randr)

;; Using the below requires refactoring a couple things
;; Eventual goal is workspaces 1-9 on "top" screen
;; and workspaces 11-19 on "bottom" screen
;; s-[1-9] should switch between workspaces 1-9 on top
;; and it should switch between workspaces 11-19 on bottom
;; TODO: may need to fix a bug that causes the "top"
;;       to be fuzzy.  Requires disconnecting the screen
;;       deleting it and re-connecting it
(defun exwm-change-screen-hook ()
  (message "exwm change screen hook called")
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--output" default-output "--auto")
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1) "--auto"
         "--output" default-output "--auto" "--below" (match-string 1))
        (setq exwm-randr-workspace-output-plist (list 1 (match-string 1)))))))

(add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)
(exwm-randr-enable)
