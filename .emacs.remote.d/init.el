;;; package --- summary  -*- mode: emacs-lisp -*-
;;; init.el --- emacs config

;;; Commentary:
;;
;; self contained Emacs config (with elpa dir)
;; should use together with tmux on remote machines
;;
;; better to use with Emacs daemon, add the following to ~/.bashrc
;; export ALTERNATE_EDITOR=''
;; alias emacs='emacsclient -t'
;; alias e=emacs
;; alias kill-emacs="emacsclient -e '(save-buffers-kill-emacs)'"
;;
;; Emacs 24+ recommended

;;; Code:

;; disable start-up message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "colinxy")

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; less frequent garbage collection
(setq gc-cons-threshold (* 50 1024 1024))
(setq gc-cons-percentage 0.6)
(run-with-idle-timer
 1 nil
 (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;; backup files
(setq backup-directory-alist '(("." . "~/.saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 2
      version-control t)

;; substitute y-or-n-p for yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; smooth scrolling
;; keyboard
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
;; mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t)       ; scroll window under mouse

;; do not blink cursor
(blink-cursor-mode -1)
;; make cursor blink as few as possible
;; (setq blink-cursor-blinks 1)
;; show prefix key in echo area quicker
(setq echo-keystrokes 0.1)

;; do not indent with tabs
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; some keys are easy to mispress
(global-unset-key (kbd "C-o"))
;; (global-unset-key (kbd "M-)"))
(global-unset-key (kbd "C-x C-n"))

;; ;; C-w : kill current line
;; (defun slick-cut (beg end &optional region)
;;   "When called interactively with no active region, kill a single line instead.
;; BEG END REGION"
;;   (interactive
;;    (if mark-active
;;        (list (region-beginning) (region-end))
;;      (list (line-beginning-position) (line-beginning-position 2)))))
;; (advice-add 'kill-region :before #'slick-cut)
;; ;; M-w : copy current line
;; (defun slick-copy (beg end &optional region)
;;   "When called interactively with no active region, copy a single line instead.
;; BEG END REGION"
;;   (interactive
;;    (if mark-active
;;        (list (region-beginning) (region-end))
;;      (message "Copied line")
;;      (push-mark)                 ;avoid point jumping to previous mark
;;      (list (line-beginning-position) (line-beginning-position 2)))))
;; (advice-add 'kill-ring-save :before #'slick-copy)

;; portable in older emacs without `advice-add'
(defadvice kill-region (before click-cut (beg end &optional region))
  "When called interactively with no active region, kill a single line instead.
BEG END REGION"
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))
(ad-activate 'kill-region)

(defadvice kill-ring-save (before slick-copy (beg end &optional region))
  "When called interactively with no active region, copy a single line instead.
BEG END REGION"
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (push-mark)                 ;avoid point jumping to previous mark
     (list (line-beginning-position) (line-beginning-position 2)))))
(ad-activate 'kill-ring-save)

;; show line number and column number
(column-number-mode t)
;; show parens without delay
(custom-set-variables '(show-paren-delay 0.0))
(show-paren-mode 1)
(global-hl-line-mode)
;; (setq line-number-display-limit-width 5) ; line number in mode line
;; line-number-mode-hook

(transient-mark-mode t)

;; use DEL to delete selected text
(delete-selection-mode 1)

;; kill the entire line (including the newline character)
(setq kill-whole-line t)

;; consider CamelCase to be 2 words
;; subword minor mode, bind it to a mode hook

;; modifier key
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank))

;; auto insert pair
;; M-( ; insert ()
(setq parens-require-spaces nil)
(electric-pair-mode 1)

;; upcase/downcase region
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; delete trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; final newline
(setq require-final-newline t)

;; vc
(setq vc-follow-symlinks t)

;; some forgotten navigation and marking commands
;; C-M-f   `forward-sexp'
;; C-M-b   `backward-sexp'
;; C-M-SPC `mark-sexp'

;; isearch magic
;; IN isearch-mode-map
;; C-w   `isearch-yank-word-or-char'
;; M-s C-e `isearch-yank-line'
;; C-M-w `isearch-del-char'
;; C-M-y `isearch-yank-char'
;; M-s e `isearch-edit-string'
;; M-s o `isearch-occur': run occur on last search string
;; M-c   `isearch-toggle-case-fold'
;; M-r   `isearch-toggle-regexp'
(define-key isearch-mode-map (kbd "C-d") 'isearch-forward-symbol-at-point)
;; or M-s . outside of isearch mode

;; compile
(global-set-key (kbd "M-g M-c") 'compile)

;; word count
(global-set-key (kbd "M-s M-c") 'count-words)

(setq split-width-threshold 150) ;split horizontally if at least <> columns

;; for window
(when tool-bar-mode
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (not window-system)
  (menu-bar-mode -1))

;; move between windows
(if (display-graphic-p)
    (cond ((eq system-type 'darwin)
           (global-unset-key (kbd "s-q"))
           (global-set-key (kbd "s-<up>") 'windmove-up)
           (global-set-key (kbd "s-<down>") 'windmove-down)
           (global-set-key (kbd "s-<left>") 'windmove-left)
           (global-set-key (kbd "s-<right>") 'windmove-right))
          ((eq system-type 'gnu/linux)
           (global-set-key (kbd "C-<up>") 'windmove-up)
           (global-set-key (kbd "C-<down>") 'windmove-down)
           (global-set-key (kbd "C-<left>") 'windmove-left)
           (global-set-key (kbd "C-<right>") 'windmove-right)))
  ;; terminal
  (windmove-default-keybindings))

(unless window-system
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right]))



;; package.el
(require 'package nil t)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; http://cachestocaches.com/2015/8/getting-started-use-package/
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'diminish)
             (package-installed-p 'bind-key))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'bind-key))

(require 'use-package)
(require 'diminish)
(require 'bind-key)
;; (setq use-package-always-ensure t)
;; (setq use-package-verbose t)

;; hide useless strings from modeline
(diminish 'abbrev-mode)

;; auto revert if file changes on disk
(use-package autorevert
  :defer 1
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))

(use-package view
  :bind (("C-v" . View-scroll-half-page-forward)
         ("M-v" . View-scroll-half-page-backward)
         ;; use with prefix argument: C-u 50 C-%
         ("C-%" . View-goto-percent)))

(use-package recentf
  :defer 1
  :custom
  (recentf-max-saved-items 500))

(use-package which-func
  :defer 1
  :config
  (which-function-mode t)
  (defun my/which-function ()
    "Calls which-function, which is not marked interactive."
    (interactive)
    (message (which-function)))
  (defun my/which-function-header-line ()
    "Move which-function to header line.

Reference: http://emacsredux.com/blog/2014/04/05/which-function-mode/"
    (interactive)
    (setq-default header-line-format
                  '((which-func-mode ("" which-func-format " "))))
    (setq mode-line-misc-info
          ;; remove which function mode from the mode line
          (assq-delete-all 'which-func-mode mode-line-misc-info)))
  (defun my/which-function-mode-line ()
    "Restore which-function to mode line."
    (interactive)
    (setq-default header-line-format nil)
    (add-to-list 'mode-line-misc-info
                 '(which-func-mode ("" which-func-format " ")))))

;;; ediff
(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; create a new frame for ediff
  (add-hook 'ediff-before-setup-hook 'make-frame)
  (add-hook 'ediff-quit-hook 'delete-frame))


;; with modification, from https://www.emacswiki.org/emacs/DavidBoon#toc4
(defun my/dired-ediff-marked-files ()
  "Run ediff on marked files."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (cond ((= (safe-length marked-files) 2)
           (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
          ((= (safe-length marked-files) 3)
           (ediff3 (nth 0 marked-files)
                   (nth 1 marked-files)
                   (nth 2 marked-files)))
          ;; inspired by dired-diff
          ((= (safe-length marked-files) 1)
           ;; prompt for another file if only 1 selected
           (let* ((current-file (nth 0 marked-files))
                  (other-file
                   (read-file-name (format "Ediff %s with: " current-file)
                                   (dired-current-directory)
                                   ;; use file on current line as default
                                   (ignore-errors (dired-get-filename))
                                   t))) ;must be valid filename
             (ediff-files current-file other-file)))
          (t (message "Mark no more than 3 files to ediff")))))

(use-package dired
  :defer t
  :ensure nil
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("=" . my/dired-ediff-marked-files)
         ;; needs dired+
         ;; ("C-t C-t" . diredp-image-dired-display-thumbs-recursive)
         )
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)

  ;; BSD ls does not support --dired
  (use-package ls-lisp
    :ensure nil
    :if (not (eq system-type 'gnu/linux))
    :config (setq ls-lisp-use-insert-directory-program nil))
  (use-package dired-aux
    :ensure nil
    :config (setq dired-isearch-filenames 'dwim))
  (use-package dired-narrow
    :defer t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))
  (use-package dired-subtree
    :defer t
    :bind (:map dired-mode-map
                ("i" . dired-subtree-insert)
                ("k" . dired-subtree-remove)))
  )


;; for fuzzy matching
(use-package flx
  :defer t)
(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-occur-mode-map
         ("n" . ivy-occur-next-line)
         ("p" . ivy-occur-previous-line))
  :init
  (ivy-mode 1)
  :custom
  (ivy-height 6)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-format-function 'ivy-format-function-line))
;; when minibuffer is active:
;; C-o     `hydra-ivy/body'
;; C-M-j   `ivy-immediate-done'
;; C-c C-o `ivy-occur': put the current candidates into a new buffer
;;                      useful with counsel-projectile-rg
;; M-n     `ivy-next-history-element': also picks thing-at-point
;; M-j     `ivy-yank-word': insert sub-word at point

(use-package counsel
  :after ivy
  :bind (("M-x"   . counsel-M-x)
         ("C-c g" . counsel-git-grep)
         ("C-c i" . counsel-imenu)))

;;; jump to position within visible text
(use-package avy
  :defer t
  :bind ("C-;" . avy-goto-char-2))

(use-package projectile
  :defer t
  :diminish projectile-mode)

(use-package counsel-projectile
  :bind (;; ("C-c f" . counsel-projectile-find-file)
         ("C-c s" . counsel-projectile-ag)
         ("C-c b" . counsel-projectile-switch-to-buffer)))

;; for finding files, use ffip
(use-package find-file-in-project
  :defer t
  :bind (("C-c f" . find-file-in-project-by-selected)
         ("C-c v" . find-file-with-similar-name)))


;;; undo tree
;; C-x u     `undo-tree-visualize'
;; C-_  C-/  `undo-tree-undo'
;; M-_  C-?  `undo-tree-redo'
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :defer 1
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode))


;; imenu
(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan t)
  (defun my/imenu-rescan ()
    "Force imenu rescan by flushing imenu cache."
    (interactive)
    (setq imenu--index-alist nil)))

;;; popup-imenu
(use-package popup-imenu
  :defer t
  :bind ("M-s M-i" . popup-imenu)
  :config (setq popup-imenu-style 'indent))

;;; imenu-list
(use-package imenu-list
  :defer t
  :bind ("M-s M-l" . imenu-list-smart-toggle)
  :config (setq imenu-list-focus-after-activation t))

;;; imenu-anywhere
(use-package imenu-anywhere
  :defer t
  :bind ("M-s M-a" . imenu-anywhere))


;;; magit
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))


(use-package company
  :defer t
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.05)
  (setq company-tooltip-align-annotations t))
;; company-dabbrev-code completes in code
;; company-dabbrev completes in comments/strings
(use-package company-dabbrev
  :defer t
  :ensure company
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t))
(use-package company-dabbrev-code
  :defer t
  :ensure company
  :config
  ;; (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-everywhere t))
(use-package company-clang
  :defer t
  :ensure company
  :config
  (add-hook 'c++-mode-hook
            (lambda () (setq company-clang-arguments '("-std=c++11")))))
(defun my/company-enable-dabbrev ()
  "Enable company dabbrev on demand.
Might be useful for modes not in `company-dabbrev-code-modes'."
  (interactive)
  (add-to-list 'company-backends '(company-capf company-dabbrev)))


(use-package flycheck
  :defer t
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++11"))))


;; c/c++
(setq-default c-basic-offset 4
              c-default-style "k&r")

;; python
;; stolen from elpy
(defun elpy-occur-definitions ()
  "Display an occur buffer of all definitions in the current buffer.
Also, switch to that buffer."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^ *\\(def\\|class\\) "))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))
;; (add-hook 'occur-hook 'occur-rename-buffer)
(use-package python
  :defer t
  :bind ("C-c C-o" . elpy-occur-definitions)
  :config
  (setq-default python-indent-offset 4)
  ;; sane defaults from elpy
  ;; https://github.com/jorgenschaefer/elpy/blob/1.22.0/elpy.el#L3347
  (set (make-local-variable 'forward-sexp-function) nil)
  (set (make-local-variable 'comment-inline-offset) 2)
  ;; pdb
  (setq gud-pdb-command-name "python3 -m pdb"))


;; themes
(use-package ample-theme
  :ensure ample-theme
  :init
  (load-theme 'ample t))


;; custom file load at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;; init.el ends here
