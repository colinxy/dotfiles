;;; init.el --- My emacs configuration

;;; Commentary:
;;
;; Flycheck made me do this
;;
;; Install Emacs with cocoa on Mac OSX
;; $ brew install Emacs --with-cocoa --with-librsvg --with-gnutls --with-imagemagick
;; Or use emacs-mac-port by railwaycat: better retina support, better scrolling
;; $ brew install emacs-mac --with-gnutls --with-imagemagick --with-modules --with-xml2 --with-modern-icon
;;
;; Install Emacs on ubuntu (build from source)
;; $ apt-get build-dep Emacs
;;
;;
;; When starting Emacs for the first time, uncomment
;; (setq use-package-always-ensure t), and all the packages will be
;; installed automatically.
;; Uncomment (setq use-package-verbose t) to debug package loading.
;;
;;; Code:


;;----------------;;
;;; basic config ;;;
;;----------------;;

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
     (push-mark)                 ;avoid point jumping to previous mark
     (list (line-beginning-position) (line-beginning-position 2)))))
(advice-add 'kill-ring-save :before #'slick-copy)

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
;; `insert-pair'
;; (add-to-list 'insert-pair-alist (list ?\$ ?\$))
;; (global-set-key (kbd "M-$") 'insert-pair)
;; M-$ conflicts with `ispell-word'

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

;;----------------------;;
;;; windows and moving ;;;
;;----------------------;;

(setq split-width-threshold 150) ;split horizontally if at least <> columns

;; for window
(when tool-bar-mode
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))
(when window-system
  (setq frame-title-format "%b")
  ;; set fonts using X resources on Linux

  ;; toggle-frame-maximized binded with M-<f10>
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
  ;; toggle-frame-fullscreen binded with <f11> (default)
  (when (eq system-type 'darwin)
    (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)))

;; window management

;; TODO: move all platform to shift prefixed key binding

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


;;-------------------;;
;;; package manager ;;;
;;-------------------;;

;;; package.el
(unless (require 'package nil t)
  (load "~/.emacs.d/elpa/package.el"))
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

;; EasyPG Assistant
(use-package epa
  :defer 5
  ;; enter passphrase through the minibuffer
  :config (setq epa-pinentry-mode 'loopback))

;; ibuffer instead of buffer list
(use-package ibuffer
  :defer t
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("U" . ibuffer-unmark-all))
  :config
  ;; from http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
  ;; automatically keeps the buffer list up to date
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode))
(use-package ibuffer-vc
  :defer t
  :init
  ;; NOTE: ibuffer-hook, not ibuffer-mode-hook, runs whenever ibuffer is called
  (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)
  (add-hook 'ibuffer-hook 'ibuffer-do-sort-by-filename/process))


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
  ;; (use-package dired-narrow
  ;;   :defer t
  ;;   :bind (:map dired-mode-map
  ;;               ("/" . dired-narrow)))
  (use-package dired-subtree
    :defer t
    :bind (:map dired-mode-map
                ("i" . dired-subtree-insert)
                ("k" . dired-subtree-remove)))
  (use-package dired-filter
    :defer t
    :bind (:map dired-mode-map
                ("," . dired-filter-mode)
                ("." . dired-filter-group-mode))
    :config
    (setq dired-filter-group-saved-groups
          '(("default"
             ("PDF" (extension . "pdf"))
             ("LaTeX" (extension "tex" "bib" "sty"))
             ("Org" (extension . "org"))
             ("Archives" (extension "zip" "rar" "gz" "bz2" "tar"))))))
  )


(use-package dired-sidebar
  :defer t
  :bind ("C-x C-d" . dired-sidebar-toggle-sidebar)
  :commands dired-sidebar-toggle-sidebar
  :custom
  (dired-sidebar-use-custom-font nil)
  (dired-sidebar-width 18))


;;; ag (silver searcher)
(use-package ag
  :defer t
  :commands ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-window t)
  (ag-reuse-buffers t))


;;; quickrun
(use-package quickrun
  :defer t
  :commands quickrun
  :config
  (quickrun-add-command "c++/c11"
    '((:command . "g++")
      (:exec    . ("%c -std=c++11 %o -o %n %s"
                   "%n %a"))
      (:remove  . ("%n")))
    :default "c++"))


;; tramp eshell: respect $PATH on remote host
(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


(use-package logview
  :defer t)


(use-package org
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  :init
  (setq org-agenda-files '("~/org"))
  (setq org-default-notes-file "~/org/notes.org")
  :config
  (setq org-src-fontify-natively t)
  (setq org-fontify-whole-heading-line t)
  (setq org-src-tab-acts-natively t)
  (setq org-highlight-latex-and-related '(latex script entities))
  ;; If you set this variable to the symbol `{}', the braces are
  ;; *required* in order to trigger interpretations as sub/superscript.
  (setq org-use-sub-superscripts '{})
  (setq org-support-shift-select t)
  ;; (setq org-capture-templates '())
  ;; prettify symbols, toggle with: C-c C-x \ (`org-toggle-pretty-entities')
  ;; entities can be found in variable `org-entities'
  (setq org-pretty-entities t)
  ;; C-c C-x C-l `org-preview-latex-fragment'
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  )
(use-package ox-latex                   ;export to latex
  :defer t
  :ensure org
  :config
  (setq org-export-with-sub-superscripts '{})
  ;; (setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))
  ;; more document class
  (add-to-list 'org-latex-classes
               ;; extarticle: more font sizes
               ;; 8pt, 9pt, 10pt, 11pt, 12pt, 14pt, 17pt, 20pt
               ;; #+LATEX_CLASS: extarticle
               '("extarticle" "\\documentclass[12pt]{extarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; C-c C-o  `org-open-at-point'

;;; org latex export code with minted
(defun my/org-export-minted ()
  "Org latex export code section with minted package."
  (interactive)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))


;;; interactively do things (ido)
;; (use-package ido
;;   :init (ido-mode 1)
;;   :bind ("C-x C-v" . ff-find-other-file)
;;   :functions ido-everywhere
;;   :config
;;   (setq ido-enable-flex-matching t)
;;   (ido-everywhere t))

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


;;; winner-mode
;; C-c <left>  `winner-undo'
;; C-c <right> `winner-redo'
(use-package winner
  :defer 5
  :config (winner-mode 1))


;; clipboard problems
(use-package xclip
  :defer 1
  :if (cond ((eq system-type 'darwin)
             (executable-find "pbcopy"))
            ((eq system-type 'gnu/linux)
             (executable-find "xclip")))
  :config (xclip-mode 1))


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
;; TODO: counsel-imenu has this feature already
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


(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))


(use-package doc-view
  :defer t
  :config
  (setq doc-view-continuous t))
;; pdf-tools binary (epdfinfo) installed from homebrew
;;; pdf-tools
(use-package pdf-tools
  :defer t
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
;; (pdf-tools-install)                     ;too slow, load on demand


;;; shell integration
;; M-x eshell : quick shell session with lisp capability
;; M-x term   : (or ansi-term) available for eshell-visual-command
(use-package term
  :defer t
  :bind (:map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         ("C-y" . term-paste))
  :config
  (ansi-color-for-comint-mode-on))
;; C-c C-j  `term-line-mode'   edit as in emacs
;; C-c C-k  `term-char-mode'  "raw mode", just term commands
(use-package eshell
  :defer t
  :defines (eshell-hist-ignoredups eshell-visual-commands)
  :config
  ;; BUT they make emacs slow (emacs buffers I/O)
  ;; switch to term when executing these commands
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "tail")
  (add-to-list 'eshell-visual-commands "htop")
  ;; eshell history
  (setq eshell-hist-ignoredups t)
  (setq eshell-scroll-to-bottom-on-input t))
;; the biggest limitation to eshell is that it runs in TERM=dumb
;; C-c C-p  `eshell-previous-prompt'   C-c C-n  `eshell-next-prompt'
;; C-c C-b  `eshell-backward-argument' C-c C-f  `eshell-forward-argument'
;; C-c C-r  `eshell-show-output'
;; C-c C-o  `eshell-kill-output'
;; C-c C-t  `eshell-truncate-buffer'
;; C-c C-u  `eshell-kill-input'  like C-u
;; M-p      `eshell-previous-matching-input-from-input'  also <up>
;; M-r      `eshell-previous-matching-input'
;; C-c M-b  `eshell-insert-buffer-name'

;;; mutiple cursor
;; Shift key does not work for terminal
(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)


;;; highlight TODO/FIXME
(use-package fic-mode
  :defer t
  :hook (prog-mode . my/fic-small-file)
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "XXX"))
  (defun my/fic-small-file ()
    "Fontifying a large C++ file is slow."
    (unless (and (derived-mode-p 'c-mode 'c++-mode)
                 (> (buffer-size) (* 10 1024)))
      (fic-mode))))


;;; YASnippet
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :hook ((java-mode . yas-minor-mode)
         (c++-mode  . yas-minor-mode))
  :config
  (yas-reload-all))

;;; async compilation of melpa package
(use-package async-bytecomp
  :defer 3
  :ensure async
  :config
  (setq async-bytecomp-allowed-packages '(all))
  (async-bytecomp-package-mode 1))

;;; magit
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

;;; dumb-jump: jump to definition based on regexp
(use-package dumb-jump
  :defer t
  :bind (("M-s M-." . dumb-jump-go)
         ("M-s M-," . dumb-jump-back)
         ("M-s M-o" . dumb-jump-go-other-window)
         ("M-s M-h" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-default-project ".")  ;default project root dir
  ;; (setq dumb-jump-selector 'ivy)
  )


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
  :config (setq company-dabbrev-downcase nil))
(use-package company-clang
  :defer t
  :ensure company
  :config
  (add-hook 'c++-mode-hook
            (lambda () (setq company-clang-arguments '("-std=c++11")))))
(defun my/company-enable-dabbrev ()
  "Enable company dabbrev on demand."
  (interactive)
  (add-to-list 'company-backends '(company-capf company-dabbrev)))


(use-package flycheck
  :defer t
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++11"))))


;; GNU Global
;; generate tags with: gtags --gtagslabel=ctags  # or pygments
;; https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btags/gtags
(use-package ggtags
  :bind (:map ggtags-mode-map
         ("M-?" . ggtags-find-reference)
         :map ggtags-global-mode-map
         ;; also kill buffer
         ("q" . my/quit-window-kill-buffer))
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)
                (setq-local eldoc-documentation-function
                            #'ggtags-eldoc-function)
                (setq-local imenu-create-index-function
                            #'ggtags-build-imenu-index))))
  :config
  (defun my/quit-window-kill-buffer ()
    (interactive)
    (quit-window t)))


;;; cc-mode: mode for editing c/c++/java/awk
(use-package cc-mode
  :defer t
  ;; :mode ("\\.h\\'" . c++-mode)
  :hook (java-mode . subword-mode)
  :config
  (setq-default c-basic-offset 4
                c-default-style "k&r")
  (c-set-offset 'innamespace [0])
  (use-package gdb-mi
    :defer t
    :config
    (setq gdb-many-windows t
          gdb-show-main t)))

;;; irony-mode, irony-eldoc, company-irony, flycheck-irony
;; install-server compilation flags:
;; cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_INSTALL_PREFIX=$HOME/.emacs.d/irony/ $HOME/.emacs.d/elpa/irony-{latest}/server && cmake --build . --use-stderr --config Release --target install
;; requires libclang
(use-package irony
  :defer t
  ;; :init
  ;; (add-hook 'c++-mode-hook 'irony-mode)
  ;; (add-hook 'c-mode-hook 'irony-mode)
  ;; LOAD IRONY MODE ON DEMAND WITH M-x irony-mode
  :config
  ;; make irony aware of .clang_complete or cmake
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; company version >= `0.8.4' include these commands by default
  ;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (use-package irony-eldoc
    :defer t
    :init (add-hook 'irony-mode-hook 'irony-eldoc))
  (use-package company-irony
    :defer t
    :init (add-to-list 'company-backends 'company-irony))
  (use-package company-irony-c-headers
    :defer t
    :init (add-to-list 'company-backends 'company-irony-c-headers))
  (use-package flycheck-irony
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
  )


;; a java development environment that just works
(use-package meghanada
  :defer t)
;; load meghanada on demand
(defun my/java-meghanada ()
  "Start meghanada on demand."
  (interactive)
  (meghanada-mode t)
  (add-hook 'java-mode-hook (lambda () (meghanada-mode t))))

;; groovy-mode works well for gradle
(use-package groovy-mode
  :defer t)


;;; Python
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  (setq-default python-indent-offset 4)
  (setq gud-pdb-command-name "python3 -m pdb")
  ;; problem with ipython 5+ prompt
  (setq python-shell-interpreter "ipython3"
        ;; ipython with readline: https://github.com/ipython/rlipython
        ;; pip3 install rlipython
        python-shell-interpreter-args "-i --TerminalIPythonApp.interactive_shell_class=rlipython.TerminalInteractiveShell"))
;; C-c <   `python-indent-shift-left'
;; C-c >   `python-indent-shift-right'

;; M-x elpy-enable : start elpy on demand with
(use-package elpy
  :after (python)
  :config
  (setq elpy-rpc-python-command "python3")
  (define-key elpy-mode-map (kbd "C-<left>") nil)
  (define-key elpy-mode-map (kbd "C-<right>") nil)
  (define-key elpy-mode-map (kbd "C-<up>") nil)
  (define-key elpy-mode-map (kbd "C-<down>") nil)
  ;; prefer flycheck to flymake
  (remove-hook 'elpy-modules 'elpy-module-flymake))


;;; Common Lisp
;; M-x slime
;; slime handles indent correctly
;; (setq lisp-indent-function 'common-lisp-indent-function)
(use-package rainbow-delimiters
  :defer t
  :hook ((lisp-mode       . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (json-mode       . rainbow-delimiters-mode)))
(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  ;; requires slime-company
  (setq slime-contribs '(slime-fancy slime-company))
  (use-package slime-company
    :defer t))

;;; Scheme
;; M-x run-geiser
(use-package geiser
  :defer t
  :defines (geiser-active-implementations)
  ;; racket has great documentation
  :config
  (setq geiser-active-implementations
        '(racket
          ;; guile
          ))
  )
;; raco pkg install compatibility-lib --auto  # not so complete
;; raco pkg install drracket                  # everything


(use-package rust
  :defer t
  :hook (rust-mode . flycheck-rust-setup))
(use-package flycheck-rust
  :after (rust))
;; could be slow (compiling stuff), load racer on demand
(use-package racer
  :hook ((racer-mode . eldoc-mode)
         (racer-mode . company-mode))
  :config
  (setq racer-rust-src-path
        (concat (string-trim (shell-command-to-string "rustc --print sysroot"))
                "/lib/rustlib/src/rust/src")))
(defun my/rust-racer ()
  "Start rust racer on demand."
  (interactive)
  (racer-mode)
  (add-hook 'rust-mode-hook racer-mode))


;;; OCaml
;; tuareg, merlin (minor mode)
;; install opam
(use-package tuareg
  ;; opam install tuareg
  :defer t)
(use-package merlin
  ;; opam install merlin
  :defer t
  :hook (tuareg-mode . merlin-mode)
  ;; merlin-company added by merlin.el
  )
(use-package utop
  ;; opam install utop
  :defer t
  :config
  ;; `opam config exec' doesn't work
  (setq utop-command "sh -c \"eval $(opam config env) && utop -emacs\""))


;;; Ruby
;; inf-ruby: repl integration
;; (inf-ruby-console-auto)
;; M-x inf-ruby (or C-c C-s) to start ruby process
;; robe-mode: code navigation, completion
;; M-x robe-start (after inf-ruby is running)
;; then C-c C-l to load current ruby file
;; (add-hook 'ruby-mode-hook 'subword-mode)
(use-package ruby-mode
  :defer t
  :bind (:map ruby-mode-map
         ("C-M-p" . backward-list)
         ("C-M-n" . forward-list))
  :hook (ruby-mode . eldoc-mode))


;;; Javascript & HTML & CSS

(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  ;; :init (add-hook 'js2-mode-hook 'subword-mode)
  :config
  (setq js-indent-level 2)
  ;; flycheck support for eslint
  ;; flycheck will automatically load eslint if .eslintrc exists
  ;; (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; prefer flycheck warnings to js2-mode warnings
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

;;; web-mode
(use-package web-mode
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.xml\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;; web dev extra
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  ;; template engine detection (put engine:<engine> at top of file)
  (setq web-mode-enable-engine-detection t)
  ;; html entities
  (setq web-mode-enable-html-entities-fontification t)
  ;; highlight
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  ;; keybinding within current tag
  (define-key web-mode-map (kbd "M-n") 'web-mode-tag-next)
  (define-key web-mode-map (kbd "M-p") 'web-mode-tag-previous))


;;; Markdown
(use-package markdown-mode
  :defer t
  :config
  (setq markdown-command
        "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments"))
;; (add-hook 'markdown-mode-hook 'flyspell-mode)


;; LaTeX
;;; AUCTeX
(use-package tex
  :defer t
  :ensure auctex
  :functions (TeX-revert-document-buffer)
  :hook ((LaTeX-mode . outline-minor-mode)
         ;; Update PDF buffers after successful LaTeX runs
         (TeX-after-TeX-LaTeX-command-finished . TeX-revert-document-buffer)
         ;; synctex
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . turn-on-reftex)
         (TeX-mode   . prettify-symbols-mode))
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)                 ;pdflatex
  ;; math insert by pair $$, \(\)
  (setq TeX-electric-math '("$" . "$"))
  ;; synctex
  (setq TeX-source-correlate-method 'syntex)
  ;; use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (use-package latex
    :defer t
    :ensure auctex
    :config
    ;; insert {}, [], ()
    (setq LaTeX-electric-left-right-brace t))

  (use-package reftex
    :defer t
    :config (setq reftex-plug-into-AUCTeX t))
  )
;; C-M-a   `LaTeX-find-matching-begin'
;; C-M-e   `LaTeX-find-matching-end'
;; C-c =   `reftex-toc' (table of contents, sections)

;; C-c C-a `TeX-command-run-all' (compile all and view)
;; C-c ~   `LaTeX-math-mode'
;; C-c C-m `TeX-insert-macro'
;; C-c C-e `LaTeX-environment' (insert environment)
;; C-u C-c C-e change current environment (with prefix arg)
;; C-c .   `LaTeX-mark-environment'
;; C-c *   `LaTeX-mark-section'
;; C-c ]   `LaTeX-close-environment'
;; "C-c C-j" or "M-RET" `LaTeX-insert-item'

;; C-c C-f     `TeX-font'
;; C-c C-f C-b (bold text) \textbf{}
;; C-c C-f C-i (italics text) \textit{}
;; C-c C-f C-e (emphasized text) \emph{}
;; C-c C-f C-t (typewriter text) \texttt{}

;; C-c C-p C-p `preview-at-point'
;; C-c C-p C-e `preview-environment'
;; C-c C-p C-r `preview-region'


(defun my/latex-setup ()
  "To be set as mode hook for latex and org modes."
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))
(use-package company-math
  :defer t
  :hook ((org-mode . my/latex-setup)
         (TeX-mode . my/latex-setup))
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))


;;; yaml mode
(use-package yaml-mode
  :defer t
  :bind (:map yaml-mode-map
         ("C-m" . newline-and-indent))
  :config
  ;; requires highlight-indentation
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  ;; ansible minor mode
  ;; https://github.com/k1LoW/emacs-ansible
  (add-hook 'yaml-mode-hook 'ansible)
  ;; requires ansible-doc-mode, https://github.com/lunaryorn/ansible-doc.el
  ;; (add-hook 'yaml-mode-hook 'ansible-doc-mode)
  (use-package company-ansible
    :defer t
    :init (add-to-list 'company-backends 'company-ansible)))


;;; gnuplot mode
;; (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(use-package gnuplot-mode
  :defer t
  :mode "\\.gp\\'")


;;; emacs theme and modeline

;; allow command line switches to choose startup theme
(defvar my-theme-dark
  (expand-file-name "theme-dark.el" user-emacs-directory))
(defvar my-theme-light
  (expand-file-name "theme-light.el" user-emacs-directory))
(defvar my-theme-modern
  (expand-file-name "theme-modern.el" user-emacs-directory))
(defvar my-theme-terminal
  (expand-file-name "theme-terminal.el" user-emacs-directory))
(cond ((and (member "-dark" command-line-args)
            (file-exists-p my-theme-dark))
       (message "Loading dark theme ...")
       (load-file my-theme-dark))
      ((and (member "-light" command-line-args)
            (file-exists-p my-theme-light))
       (message "Loading light theme ...")
       (load-file my-theme-light))
      ((and (member "-modern" command-line-args)
            (file-exists-p my-theme-modern))
       (message "Loading modern theme ...")
       (load-file my-theme-modern))
      ((and (member "-terminal" command-line-args)
            (file-exists-p my-theme-terminal))
       (message "Loading terminal theme ...")
       (load-file my-theme-terminal))
      ((and window-system
            (file-exists-p my-theme-dark)) ;by default use dark theme
       (message "Loading dark theme ...")
       (load-file my-theme-dark))
      ((file-exists-p my-theme-terminal)
       (message "Loading terminal theme ...")
       (load-file my-theme-terminal))
      (t
       (load-theme 'tango-dark t)))
(defun my/themes (_theme)
  "Command line switch placeholder.")
(add-to-list 'command-switch-alist '("dark" . my/themes))
(add-to-list 'command-switch-alist '("light" . my/themes))
(add-to-list 'command-switch-alist '("modern" . my/themes))
(add-to-list 'command-switch-alist '("terminal" . my/themes))


;; custom file load at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;; init.el ends here
