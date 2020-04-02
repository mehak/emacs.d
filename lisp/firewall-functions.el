;;; -*- lexical-binding: t; -*-

;; Collection of functions for working with firewalls in an easy way

;; TODO re-factor to make more clear
(defun get-firewall-conf (firewall configuration-file configuration-commands)
  "Connect to a firewall, grab the configuration and copy it to configuration-file"
  (require 'term)
  (let* ((cmd "/usr/bin/ssh")
         (args firewall)
         (switches (split-string-and-unquote args))
         (name (format "%s" firewall))
         (termbuf (apply 'make-term name cmd nil switches))
         (process (get-buffer-process termbuf)))
    (set-process-sentinel
     process
     (lambda (process event)
       (let ((termbuf (process-buffer process))
             (configuration-buffer (find-file configuration-file)))
         (if (string-equal event "finished\n")
             (progn
               (set-buffer termbuf)
               (copy-to-buffer configuration-buffer
                               (point-min)
                               (point-max))
               (kill-buffer termbuf)
               (set-buffer configuration-buffer)
               (search-forward "# show")
               (kill-region (point-min) (point))
               (kill-line)
               (search-forward "\[edit")
               (move-to-column 0)
               (kill-region (- (point) 1)  (point-max)))))))
    (set-buffer termbuf)
    (term-mode)
    (comint-send-string process configuration-commands)))

;; Used to get the configuration from a palo alto firewall
(defun get-pan-conf (firewall configuration-file)
  "Connect to a palo-alto firewall,
grab the configuration and copy it to configuration-file"
  (interactive "sFirewall IP: \nFConfiguration File: ")
  (get-firewall-conf
   firewall
   configuration-file
   (concat
    "set cli pager off\n"
    "set cli config-output-format set\n"
    "configure\n"
    "show\n"
    "exit\n"
    "exit\n")))
