;;; -*- lexical-binding: t; -*-

;; Show differences for specific hunk only
(setq-default magit-diff-refine-hunk t)

;; Switch to magit-status buffer instead of splitting the window/frame
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
