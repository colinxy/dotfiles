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

;; less frequent garbage collection
(setq gc-cons-threshold 10000000)        ;10MB

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

;; auto revert if file changes on disk
(global-auto-revert-mode)

;; do not indent with tabs
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; some keys are easy to mispress
(global-unset-key (kbd "C-o"))
;; (global-unset-key (kbd "M-)"))

;; https://emacs.stackexchange.com/questions/2347/kill-or-copy-current-line-with-minimal-keystrokes
;; C-w : kill current line
(defun slick-cut (beg end &optional region)
  "When called interactively with no active region, kill a single line instead.
BEG END REGION"
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))
(advice-add 'kill-region :before #'slick-cut)
;; M-w : copy current line
(defun slick-copy (beg end &optional region)
  "When called interactively with no active region, copy a single line instead.
BEG END REGION"
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))
(advice-add 'kill-ring-save :before #'slick-copy)

(column-number-mode t)
;; show parens without delay
(setq show-paren-delay 0.0)
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
;; C-M-w `isearch-del-char'
;; C-M-y `isearch-yank-char'
;; M-c   `isearch-toggle-case-fold'
;; M-s e `isearch-edit-string'
(define-key isearch-mode-map (kbd "C-d") 'isearch-forward-symbol-at-point)
;; or M-s . outside of isearch mode

(setq split-width-threshold 150) ;split horizontally if at least <> columns

;; for window
(when tool-bar-mode
  (tool-bar-mode -1))
(when scroll-bar-mode
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
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (windmove-default-keybindings))


;; package.el
(require 'package nil t)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
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

(diminish 'abbrev-mode)

(use-package view
  :bind (("C-v" . View-scroll-half-page-forward)
         ("M-v" . View-scroll-half-page-backward)
         ;; use with prefix argument: C-u 50 C-%
         ("C-%" . View-goto-percent)))

;; compile
(global-set-key (kbd "M-g M-c") 'compile)

;; word count
(global-set-key (kbd "M-s M-c") 'count-words)

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
(defun dired-ediff-marked-files ()
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

(defun find-file-around (orig-find-file &rest args)
  "Advice `find-file'.  ORIG-FIND-FILE original find file function.  ARGS."
  (if (eq major-mode 'dired-mode)
      (let ((default-directory (dired-current-directory)))
        ;; dynamic scoping
        (apply orig-find-file args))
    (apply orig-find-file args)))

(use-package dired
  :defer t
  :ensure nil
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("=" . dired-ediff-marked-files)
         ;; needs dired+
         ;; ("C-t C-t" . diredp-image-dired-display-thumbs-recursive)
         )
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  ;; useful when dired buffer contains subtree
  ;; tied with ido
  (advice-add 'ido-find-file :around #'find-file-around)

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


;;; interactively do things (ido)
(use-package ido
  :init (ido-mode 1)
  :bind ("C-x C-v" . ff-find-other-file)
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t))


;;; undo tree
;; C-x u     `undo-tree-visualize'
;; C-_  C-/  `undo-tree-undo'
;; M-_  C-?  `undo-tree-redo'
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))


;; imenu
(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan t)
  (defun imenu-rescan ()
    "Force imenu rescan by flushing imenu cache."
    (interactive)
    (setq imenu--index-alist nil)))

;;; popup-imenu
(use-package popup-imenu
  :ensure t
  :defer t
  :bind ("M-s M-i" . popup-imenu)
  :config (setq popup-imenu-style 'indent))

(use-package which-func
  :defer t
  :init
  (require 'which-func)         ;make sure which-func-modes is defined
  (which-func-mode))


(use-package company
  :defer t
  :ensure t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1))
;; company-dabbrev-code completes in code
;; company-dabbrev completes in comments/strings
(use-package company-dabbrev
  :defer t
  :ensure company
  :config (setq company-dabbrev-downcase nil))
(use-package company-clang
  :defer t
  :ensure company
  :config
  (add-hook 'c++-mode-hook
            (lambda () (setq company-clang-arguments '("-std=c++11")))))
(defun company-enable-dabbrev ()
  "Enable company dabbrev on demand."
  (interactive)
  (add-to-list 'company-backends '(company-capf company-dabbrev)))


(use-package flycheck
  :defer t
  :ensure t
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
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
  :init
  (setq-default python-indent-offset 4)
  :bind ("C-c C-o" . elpy-occur-definitions))


;; themes
(use-package ample
  :ensure ample-theme
  :init
  (load-theme 'ample t))


;; custom file load at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
