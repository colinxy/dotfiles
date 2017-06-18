;;; theme-modern.el --- load modern theme

;;; Commentary:
;;

;;; Code:

(use-package doom-themes
  :init
  (doom-themes-visual-bell-config)
  (use-package solaire-mode
    :init
    (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
    (add-hook 'after-revert-hook #'turn-on-solaire-mode)
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
    (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))

  ;; neotree integration requires all-the-icons
  (use-package all-the-icons)
  (doom-themes-neotree-config)
  (load-theme 'doom-vibrant t))

;; enable global nlinum
(use-package nlinum
  :init (global-nlinum-mode)
  :config (setq nlinum-highlight-current-line t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'after-init-hook 'neotree-toggle)
