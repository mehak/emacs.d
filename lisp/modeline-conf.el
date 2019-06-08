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
(setq battery-mode-line-format "%p%%")
(display-battery-mode 1)

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


(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mehak-mode-line-position
   " "
   mode-line-modified
   " "
   minions-mode-line-modes
   mode-line-misc-info
   mode-line-buffer-identification
   mode-line-end-spaces))
