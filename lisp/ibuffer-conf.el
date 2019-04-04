;;; -*- lexical-binding: t; -*-

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(define-ibuffer-column exwm-class
  (:name "Class")
  (cond
   (exwm-class-name (format "%s" exwm-class-name))
   (t (format "%s" ""))))

;; Needs work to look good, major-mode is not equal to ibuffer-formats mode
(define-ibuffer-column exwm-mode
  (:name "mode")
  (cond
   ((string-equal major-mode "exwm-mode") (format "%s" exwm-class-name))
   (t (format "%s" major-mode))))

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 50 50 :left :elide)
              " "
              (size-h 16 16 :right)
              " "
              (mode 18 18 :left :elide)
              (exwm-class 18 18 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))
