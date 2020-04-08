;;; -*- lexical-binding: t -*-

;;; Thanks to Dr. Aaron S. Jackson
(defun shrug ()
  "shrugs"
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

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

(defun mehak/reset-buffers-mode-line ()
  "This function resets all buffers' mode lines to the default mode-line-format"
  (interactive)
  (let ((lb (buffer-list)))
    (mapcar
     (lambda (buffer)
       (with-current-buffer
           (get-buffer buffer)
         (setq mode-line-format
               (default-value
                 'mode-line-format))))
     lb)))

;; TODO add option for taking full screen screenshots
(defun mehak/screenshot (path)
  "Takes a variable sized screenshot.
Used to take a screenshot named PATH of a size defined by the user.

PATH may be any user defined path but defaults to
/tmp/vm/screenshot-%s.jpg where %s is the time-to-seconds of current-time."
  (interactive
   (list (read-file-name "Path: "
                         "/tmp/vm/"
                         (format "screenshot-%s.jpg"
                                 (time-to-seconds (current-time))))))
  (call-process "/usr/bin/import" nil nil nil path))

;; Add command for creating new eshell buffers
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used in eshell-conf
(defun mehak/pwd ()
  "Shows the present working directory with $HOME replaced with ~"
  (replace-regexp-in-string
   (concat "^" (getenv "HOME"))
   "~"
   (eshell/pwd)))

;; Map over commands that don't work well with eshell
(defun mehak/eshell/noop (name &rest rest)
  (message "%s called with: %s" name rest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used in modeline
(defun mehak/bond0-ip ()
  "Shows current ip assigned to bond0"
  (or (format-network-address
    (first
     (network-interface-info
      "bond0"))
    "omit-port")
      "no ip"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNction to enlarge ascii art
(defun magnify-ascii-art (ascii-art magnification-value)
  "Given ascii-art and magnification-value, enlarge image by
magnification value"
  (with-temp-buffer
    (let ((copy-character (apply 'concat
                                 (make-list
                                  magnification-value
                                  "\\&")))
          (copy-line (apply 'concat
                            (mapcar* 'concat
                                     (make-list
                                      magnification-value
                                      "\\&")
                                     (append (make-list
                                              (- magnification-value 1)
                                              "\n")
                                             '(""))))))
      (insert ascii-art)
      (goto-char (point-min))
      (flush-lines "^$")
      (goto-char (point-min))
      (replace-regexp "." copy-character)
      (goto-char (point-min))
      (replace-regexp "^.*$" copy-line)
      (buffer-string))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
