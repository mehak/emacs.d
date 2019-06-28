;;; -*- lexical-binding: t; -*-

;; Shut off pronounce for sdcv
(require 'sdcv)
(setq sdcv-word-pronounce nil)
(global-set-key (kbd "<f11>") 'sdcv-search-input)
(global-set-key (kbd "<f10>") 'sdcv-search-pointer)

(evil-define-key 'normal sdcv-mode-map
  "q" 'quit-window)
