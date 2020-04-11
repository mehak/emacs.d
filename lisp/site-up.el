;;; site-up.el --- Setup buffer to check connection to sites -*- lexical-binding: t; -*-

;; Author: Nathanael Merlin <nate@blazing.io>
;; Version: 0.0.1

(defun site-up/site-up (site up-message down-message)
  "Used inside a timer, does the actual work"
  (let ((site-up-buffer
         (get-buffer-create (concat "*site-up-" site "*")))
        (up-or-down-p (= 0
                         (call-process "ping"
                                       nil
                                       nil
                                       nil
                                       "-c" "1"
                                       "-W" "1"
                                       site)))
        (delete-then-insert (lambda (text)
                              (goto-line 1)
                              (kill-region (point) (point-max))
                              (insert text))))
    (set-buffer site-up-buffer)
    (if up-or-down-p
        (funcall delete-then-insert up-message)
      (funcall delete-then-insert down-message))
    (beginning-of-buffer)))

(setq site-up/timers
      (make-hash-table :test 'equal :size 10))

(defun new-site-up (site &optional repeat up-message down-message)
  "Make new timer & buffer to test connection to SITE"
  (interactive "sSite IP/Name: ")
  (let ((repeat (or repeat 10))
        (up-message (or up-message (magnify-banner
                                    (shell-command-to-string "banner -c# 'UP'")
                                    3)))
        (down-message (magnify-banner
                       (shell-command-to-string "banner -c# 'DOWN'")
                       3)))
    (puthash site
             (run-at-time nil
                          10
                          (lambda ()
                            (site-up/site-up site
                                             up-message
                                             down-message)))
             site-up/timers)))

(defun cancel-site-up (site)
  "Remove previously set timer by SITE"
  (interactive "sSite IP/Name: ")
  (let ((timer-object (gethash site site-up/timers))
        (site-up-buffer (concat "*site-up-" site "*")))
    (cancel-timer timer-object)
    (remhash site site-up/timers)
    (kill-buffer site-up-buffer)))

(provide 'site-up)
