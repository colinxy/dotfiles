;;; theme-terminal.el --- load terminal theme

;;; Commentary:
;; should load theme designed for terminal colors
;; i.e. 256color, 16color, 8color

;;; Code:
(use-package ample
  :ensure ample-theme
  :init
  (load-theme 'ample t))
