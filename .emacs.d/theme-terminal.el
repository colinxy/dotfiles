;;; theme-terminal.el --- load terminal theme

;;; Commentary:
;; should load theme designed for terminal colors
;; i.e. 256color, 16color, 8color

;;; Code:
(use-package ample-theme
  :ensure t
  :init
  (load-theme 'ample t))

;;; telephone-line

(use-package telephone-line
  :init
  (setq telephone-line-lhs
        '((accent . (telephone-line-buffer-segment))
          (nil    . (telephone-line-airline-position-segment))
          (accent . (telephone-line-minor-mode-segment))
          (nil    . (telephone-line-misc-info-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-vc-segment))
          (accent . (telephone-line-major-mode-segment))))
  (let* ((style (if window-system "cubed" "utf-abs"))
         (primary-left    (intern (concat "telephone-line-" style "-left")))
         (secondary-left  (intern (concat "telephone-line-" style "-hollow-left")))
         (primary-right   (intern (concat "telephone-line-" style "-right")))
         (secondary-right (intern (concat "telephone-line-" style "-hollow-right"))))
    (setq telephone-line-primary-left-separator    primary-left
          telephone-line-secondary-left-separator  secondary-left
          telephone-line-primary-right-separator   primary-right
          telephone-line-secondary-right-separator secondary-right))
  (telephone-line-mode t))


;; (require 'hl-line)
;; (set-face-attribute hl-line-face nil :background "color-16")
