;;; -*- lexical-binding: t -*-

;; Thanks to Jeff Dwork
;; https://www.emacswiki.org/emacs/ToggleWindowSplit
;; Toggle window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;; https://stackoverflow.com/a/17923238
;; Join as comma separated list
(defun lines-to-cslist (start end &optional arg)
  (interactive "r\nP")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "\"%s\"" x))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)
    (when arg (forward-char (length insertion)))))


;; Thanks to pkkm
;; https://www.reddit.com/r/emacs/comments/72ukrx/theme_preferences/dnmgcef?utm_source=share&utm_medium=web2x
(defun change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough (see <https://emacs.stackexchange.com/a/19242>).
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapcar #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anything I have written is below this point
(defun mehak/major-mode-or-class ()
  "Returns the major mode or EXWM class"
  (if (string-equal major-mode "exwm-mode")
      (format "%s" exwm-class-name)
    (format "%s" mode-name)))


;; TODO add option for taking full screen screenshots
(defun mehak/screenshot (filename)
  "Takes a variable sized screenshot"
  (interactive
   (let ((default-filename (format "/tmp/vm/screenshot-%s.jpg"
                                   (time-to-seconds (current-time)))))
     (list (read-string (format "Filename [%s]: " default-filename)
                        nil
                        nil
                        default-filename))))
  (call-process "/usr/bin/import" nil nil nil filename))
