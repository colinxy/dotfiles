;;; theme-dark.el --- load dark theme

;;; Commentary:
;;

;;; Code:

(use-package doom-themes
  :init
  (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
  (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  ;; neotree integration requires all-the-icons
  (use-package all-the-icons)
  (doom-themes-neotree-config)
  ;; enable global nlinum
  (use-package nlinum
    :init (global-nlinum-mode))
  (doom-themes-nlinum-config)
  (setq org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  (load-theme 'doom-one t))
