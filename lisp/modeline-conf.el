;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup a ruler (vim) like display at bottom
(line-number-mode 1)
(column-number-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minions - minor mode menus
(use-package minions
  :config
  (minions-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Battery mode
(setq battery-mode-line-format " ")
(display-battery-mode 0)

;; Pulled from smart-mode-line
(defface sml/global           '((t :inverse-video nil)) "")
(defface sml/charging            '((t :inherit sml/global :foreground "ForestGreen")) "")
(defface sml/discharging         '((t :inherit sml/global :foreground "Red"))         "")

(defun sml/set-battery-font ()
  "Set `sml/battery' face depending on battery state."
  (let ((data (and (boundp 'battery-status-function)
                   battery-status-function
                   (funcall battery-status-function))))
    (if  (string-equal "AC" (cdr (assoc 76 data)))
        (copy-face 'sml/charging 'sml/battery)
      (copy-face 'sml/discharging 'sml/battery))))

(defadvice battery-update (before sml/set-battery-font activate)
  "Fontify the battery display."
  (sml/set-battery-font))

(eval-after-load 'battery
  '(defadvice battery-update (after sml/after-battery-update-advice () activate)
     "Change battery color."
     (when battery-mode-line-string
       (setq battery-mode-line-string
             (propertize battery-mode-line-string
                         'face 'sml/battery)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show the time
(setq display-time-string-forms
      '((propertize (format-time-string " %F %R")
                    'face `(:foreground "dim grey"))))
(display-time-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line:Column
(setq mehak-mode-line-position
      '(line-number-mode
        ("%l"
         (column-number-mode ":%C"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default: #b294bb
(modify-face 'mode-line-buffer-id "goldenrod")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and functions for manipulating the modeline

;;  TODO figure out how to put the ip address in a variable for looks
;;       maybe something like ip-address-mode-line
(defvar mehak/modeline
  '("%e"
    mode-line-front-space
    mehak-mode-line-position
    " "
    mode-line-modified
    " "
    minions-mode-line-modes
    mode-line-misc-info
    " "
    mode-line-buffer-identification
    (:eval (propertize
            (format " %s " (mehak/get-ip-by-interface "enp12s0"))
            'face
            `(:foreground "sienna")))
    mode-line-end-spaces)
  "My normal modeline")

(defun mehak/toggle-mode-line-local ()
  "Toggle viewing the mode line for current buffer"
  (interactive)
  (if (eql mehak/modeline mode-line-format)
      (setq mode-line-format nil)
    (setq mode-line-format mehak/modeline))
  (redraw-display))

(defun mehak/toggle-mode-line ()
  "Toggle viewing the mode line"
  (interactive)
  (if (eql mehak/modeline mode-line-format)
      (setq-default mode-line-format nil)
    (setq-default mode-line-format mehak/modeline))
  (redraw-display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default mode-line-format mehak/modeline)
