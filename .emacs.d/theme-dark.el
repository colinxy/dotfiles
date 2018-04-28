;;; theme-dark.el --- load dark theme

;;; Commentary:
;; currently solarized dark with spaceline

;;; Code:

;;; solarized theme by bbatsov
;; https://github.com/bbatsov/solarized-emacs
(use-package solarized
  :ensure solarized-theme
  ;; :if window-system
  :init
  (setq x-underline-at-descent-line t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-distinct-fringe-background t)
  (setq solarized-distinct-doc-face t)
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch t)
  (setq solarized-emphasize-indicators t)
  (load-theme 'solarized-dark t))

;;; modeline colors
;; https://github.com/arranger1044/emacs.d/blob/master/rano/rano-customization.el
;; works best with dark themes
(set-face-attribute 'mode-line nil
                    :underline nil
                    :overline nil
                    :foreground "#fdf6e3"
                    :background "#2aa198"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :underline nil
                    :overline nil
                    :foreground "#fdf6e3"
                    :background "#1a655f"
                    :box nil)

;; TODO:
;; spaceline is the bottleneck to emacs init time, taking 0.5s to load

;;; spaceline
;; depends on powerline

(use-package spaceline-config
  :ensure spaceline
  :config
  ;; mac specific, see https://github.com/milkypostman/powerline/issues/54
  (when (eq system-type 'darwin)
    (setq ns-use-srgb-colorspace nil))
  (setq powerline-default-separator (if window-system 'wave 'bar))

  ;; adding page number to line-column segment
  (declare-function doc-view-current-page 'doc-view)
  (declare-function doc-view-last-page-number 'doc-view)
  (defun spaceline--docview-page-number ()
    "Display page number in doc-view mode on spaceline."
    (format "(%d/%d)"
            (eval `(doc-view-current-page))
            (doc-view-last-page-number)))
  (spaceline-define-segment line-column
    "The current line and column numbers, or `(current page/number of pages)`
  in pdf-view mode (enabled by the `pdf-tools' package) or doc-view mode."
    (cond ((eq 'pdf-view-mode major-mode) (spaceline--pdfview-page-number))
          ((eq 'doc-view-mode major-mode) (spaceline--docview-page-number))
          (t "%l:%2c")))

  (spaceline-emacs-theme)

  (spaceline-toggle-buffer-size-off)
  ;; (spaceline-toggle-minor-modes-off)
  ;; (spaceline-toggle-flycheck-error-off)
  ;; (spaceline-toggle-flycheck-warning-off)
  (spaceline-toggle-buffer-id-on)
  (spaceline-toggle-which-function-on)
  )
