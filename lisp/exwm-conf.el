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


(defun mehak/exwm-update-buffer-name ()
  "Update EXWM buffer name"
  (let ((title exwm-title))
    (if (eq (length title) 0)
        (setq title exwm-class-name))
    (exwm-workspace-rename-buffer title)))

;; Set hooks
(add-hook 'exwm-update-class-hook 'mehak/exwm-update-buffer-name)
(add-hook 'exwm-update-title-hook 'mehak/exwm-update-buffer-name)


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


;; Enable EXWM
(exwm-enable)

;; exwm-randr
(require 'exwm-randr)

(defun njm/exwm/default-only (default-display first-match-point)
  "Only show the default display"
  (goto-char first-match-point)
  (let ((regex "\n\\([^ ]+\\)")
        (xrandr-arguments `("--output" ,default-display "--auto" "--dpi" ,default-display))
        xrandr-command)
    (while (re-search-forward regex nil 'noerror)
      (if (not (string-match "VIRTUAL[0-9]+" (match-string 1)))
          (setq xrandr-arguments
                (append xrandr-arguments
                        `("--output" ,(match-string 1) "--off")))))
    (setq xrandr-command
          (append '("/usr/bin/xrandr" nil nil t)
                  xrandr-arguments))
    (message "xrandr-command: %s" xrandr-command)
    (apply 'call-process xrandr-command)))

;; Using the below requires refactoring a couple things
;; Eventual goal is workspaces 1-9 on "top" screen
;; and workspaces 11-19 on "bottom" screen
;; s-[1-9] should switch between workspaces 1-9 on top
;; and it should switch between workspaces 11-19 on bottom
(defun exwm-change-screen-hook ()
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        first-match-point
        default-output
        xrandr-command-multiple-monitors)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (setq first-match-point
            (re-search-forward xrandr-output-regexp nil 'noerror))
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (njm/exwm/default-only default-output first-match-point)
        (progn
          (setq xrandr-command-multiple-monitors
                (append '("/usr/bin/xrandr" nil nil t)
                        `("--output"
                          ,(match-string 1)
                          "--auto"
                          "--output"
                          ,default-output
                          "--auto"
                          "--below"
                          ,(match-string 1)
                          "--dpi"
                          ,(match-string 1))))
          (message "xrandr-command: %s"
                   xrandr-command-multiple-monitors)
          (apply 'call-process xrandr-command-multiple-monitors))
        (setq exwm-randr-workspace-monitor-plist (list 1 (match-string 1)))))))

(add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)
(exwm-randr-enable)
