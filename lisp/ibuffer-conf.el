;;; -*- lexical-binding: t; -*-

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size")
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
  (:name "EXWM-Mode")
  (mehak/major-mode-or-class))

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 50 50 :left :elide)
              " "
              (size-h 16 16 :right)
              " "
              (exwm-mode 18 18 :left :elide)
              " " filename-and-process)
        (mark modified read-only locked " "
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

(defun mehak/ibuffer-exwm-match (match)
  (equal (mehak/major-mode-or-class) match))

(setq ibuffer-saved-filter-groups
      '(("default"
         ("erc" (mode . erc-mode))
         ("emacs" (name . "^\\*[A-Za-z-]+\\*$"))
         ("magit" (and (name . "^magit\\(\\|-.*?\\): ")
                       (filename . ".*")))
         ("pdf-view" (mode . pdf-view-mode))
         ("zoom" (predicate . (mehak/ibuffer-exwm-match "zoom")))
         ("firefox" (predicate . (mehak/ibuffer-exwm-match "firefox"))))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
