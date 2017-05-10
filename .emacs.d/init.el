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
;; installed automatically
;;; Code:


;;----------------;;
;;; basic config ;;;
;;----------------;;

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

;; auto revert
(global-auto-revert-mode)

;; do not indent with tabs
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; some keys are easy to mispress
(global-unset-key (kbd "C-o"))
(setq outline-minor-mode-prefix (kbd "C-o"))
(global-unset-key (kbd "C-t"))
;; (global-unset-key (kbd "C-x C-w"))
(global-unset-key (kbd "M-)"))
;; C-w is only enabled when a region is selected
(defun my-kill-region ()
  "Cuts only when a region is selected."
  (interactive)
  (when mark-active
    (kill-region (region-beginning) (region-end))))
(global-set-key (kbd "C-w") 'my-kill-region)

;; show line number and column number
;; (global-linum-mode 1)  ; show line number of the left
(column-number-mode t)
(show-paren-mode 1)
(when window-system
  (global-hl-line-mode))
;; (setq line-number-display-limit-width 5) ; line number in mode line
;; line-number-mode-hook

;; use DEL to delete selected text
(delete-selection-mode 1)

;; consider CamelCase to be 2 words
;; subword minor mode, bind it to a mode hook

;; modifier key
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; auto insert pair
;; M-( ; insert ()
;; (global-set-key (kbd "M-(") 'insert-pair)
(setq parens-require-spaces nil)
(global-set-key (kbd "M-[") 'insert-pair)  ; insert []
(global-set-key (kbd "C-{") 'insert-pair)  ; insert {}
(global-set-key (kbd "M-\"") 'insert-pair) ; insert ""

;; upcase/downcase region
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; delete trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; final newline
(setq require-final-newline t)


;;----------------------;;
;;; windows and moving ;;;
;;----------------------;;

(setq split-width-threshold 150) ;split horizontally if at least <> columns

;; for window
(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))
(when window-system
  (setq frame-title-format "%b")
  ;; set font
  (cond
   ((member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil
                        :font "DejaVu Sans Mono"))
   ((member "Monaco" (font-family-list))
    (set-face-attribute 'default nil
                        :font "Monaco 14")))
  ;; for Mac OS X >= 10.7
  ;; toggle-frame-maximized binded with M-<f10>
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
  ;; toggle-frame-fullscreen binded with <f11> (default)
  ;; (set-frame-parameter nil 'fullscreen 'fullboth) ; alternative
  ;; <f11> conflicts with mac command, bind it to M-<f11>
  (when (eq system-type 'darwin)
    (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)))

;; window management

;; move between windows
;; only works for gui
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
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; http://cachestocaches.com/2015/8/getting-started-use-package/
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'diminish)
(require 'bind-key)
;; (setq use-package-always-ensure t)

;; hide useless strings from modeline
(diminish 'abbrev-mode)

;;; jump to position within visible text
(use-package avy
  :defer t
  :bind ("C-;" . avy-goto-char-2))

;; EasyPG Assistant
(use-package epa
  :defer t
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
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
  ;; (add-hook 'ibuffer-mode-hook
  ;;           (lambda ()
  ;;             (ibuffer-switch-to-saved-filter-groups "default")))
  ;; (setq ibuffer-saved-filter-groups
  ;;       '(("default"
  ;;          ("Special" (name . "^\*.*\*$")))))
  ;; (setq ibuffer-show-empty-filter-groups nil)
  )
(use-package ibuffer-vc
  :defer t
  :init
  ;; NOTE: ibuffer-hook, not ibuffer-mode-hook, runs whenever ibuffer is called
  (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)
  (add-hook 'ibuffer-hook 'ibuffer-do-sort-by-filename/process))

;; compile
(global-set-key (kbd "M-g M-c") 'compile)

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
(defun my-dired-ediff-marked-files ()
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
         ("C-s" . dired-isearch-filenames)
         ("C-M-s" . dired-isearch-filenames-regexp)
         ("=" . my-dired-ediff-marked-files)
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
  (use-package dired-narrow
    :defer t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))
  (use-package dired-subtree
    :defer t
    :bind (:map dired-mode-map
                ("i" . dired-subtree-insert)
                ("r" . dired-subtree-remove)))
  )


;;; neotree
(use-package neotree
  :defer t
  :bind ("C-x C-d" . neotree-toggle)
  :config
  (setq neo-smart-open t)
  (setq neo-autorefresh nil)
  (setq neo-theme (if window-system 'icons 'arrow)))


;;; ag (silver searcher)
(use-package ag
  :defer t
  :commands ag)


;;; quickrun
(use-package quickrun
  :defer t
  :commands quickrun)


;; tramp eshell: respect $PATH on remote host
(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


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
  (setq org-src-tab-acts-natively t)
  (setq org-highlight-latex-and-related '(latex script entities))
  ;; If you set this variable to the symbol `{}', the braces are
  ;; *required* in order to trigger interpretations as sub/superscript.
  (setq org-use-sub-superscripts '{})
  ;; (setq org-capture-templates '())
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
;; C-c C-o: org-open-at-point


;;; enable interactively do things (ido)
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere t)

;; maybe try ivy for a while ?
;; (ivy-mode 1)


;; imenu
(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan 1))

;;; popup-imenu
(use-package popup-imenu
  :defer t
  :bind ("M-s M-i" . popup-imenu)
  :config (setq popup-imenu-style 'indent))

;;; imenu-list
(use-package imenu-list
  :defer t
  :bind ("C-'" . imenu-list-smart-toggle)
  :config (setq imenu-list-focus-after-activation t))


;;; undo tree
;; C-_  C-/  (`undo-tree-undo')
;; M-_  C-?  (`undo-tree-redo')
(use-package undo-tree
  :defer t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)


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
;; M-x eshell
;; M-x shell
;; M-x term
;; M-x ansi-term
(use-package term
  :defer t
  :bind (("M-t" . ansi-term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         ("C-y" . term-paste)
         ("M-(" . my-term-send-left-paren)
         ("M-\"" . my-term-send-left-doublequote))
  :config
  (ansi-color-for-comint-mode-on)
  (defun my-term-send-left-paren ()
    (interactive)
    (term-send-raw-string "()")
    (term-send-left))
  (defun my-term-send-left-doublequote ()
    (interactive)
    (term-send-raw-string "\"\"")
    (term-send-left)))
;; C-c C-j switch to line mode
;; C-c C-k switch to char mode
(use-package eshell
  :defer t
  :config
  (use-package em-alias
    :defer t
    :ensure eshell
    :config
    (eshell/alias "vi" "find-file $1")
    (eshell/alias "ll" "ls -al $*")
    (eshell/alias "la" "ls -A $*"))
  (use-package em-hist
    :defer t
    :ensure eshell
    :config
    (setq eshell-hist-ignoredups t))
  (use-package em-term
    :defer t
    :ensure eshell
    :config
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "tail"))
  )

;; exec-path-from-shell: consistent with shell in Mac OS X
;; the current launcher em from .bashrc can let emacs inherit correct PATH
;; (when (memq window-system '(mac ns))
;;   ;; (require 'exec-path-from-shell)
;;   (exec-path-from-shell-initialize))


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
  :init (add-hook 'prog-mode-hook 'fic-mode)
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "CHECKME" "XXX")))


;;; YASnippet
(use-package yasnippet
  :defer t)

;;; async compilation of melpa package
(use-package async-bytecomp
  :defer t
  :ensure async
  :init
  (async-bytecomp-package-mode 1)
  :config
  (setq async-bytecomp-allowed-packages '(all)))

;;; magit
;; (global-set-key (kbd "C-x g") 'magit-status)
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

;;; highlight changes
(use-package diff-hl
  :defer t
  :init
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; (global-diff-hl-mode 1)
  )

;;; dumb-jump: jump to definition based on regexp
(use-package dumb-jump
  :defer t
  :bind (("M-g M-." . dumb-jump-go)
         ("M-g M-," . dumb-jump-back)
         ("M-g M-o" . dumb-jump-go-other-window)
         ("M-g M-h" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-default-project ".")  ;default project root dir
  ;; (setq dumb-jump-selector 'ivy)
  )


(use-package flycheck
  :defer t
  :ensure t
  :init (add-hook 'after-init-hook 'global-flycheck-mode))


(use-package company
  :defer t
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  ;; :diminish company-mode
  )
(use-package company-dabbrev
  :defer t
  :ensure company
  :config (setq company-dabbrev-downcase nil))


;;; cc-mode: mode for editing c/c++/java/awk
(use-package cc-mode
  :defer t
  :mode ("\\.h\\'" . c++-mode)
  :init
  (setq-default c-basic-offset 4
                c-default-style "k&r")
  :config
  (use-package c++-mode
    :defer t
    :init
    ;; do not indent namespace
    (c-set-offset 'innamespace [0]))
  (use-package gdb-mi
    :defer t
    :init
    (setq gdb-many-windows t
          gdb-show-main t)))

;;; irony-mode, irony-eldoc, company-irony, flycheck-irony
;; always use clang as compiler: brew install llvm --with-clang
;; install-server compilation flags:
;; cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_INSTALL_PREFIX\=/Users/yxy/.emacs.d/irony/ /Users/yxy/.emacs.d/elpa/irony-{latest}/server && cmake --build . --use-stderr --config Release --target install
;; requires libclang
(use-package irony
  :defer t
  ;; irony `0.3.0' breaking changes, removed async API (supported by default)
  ;; now irony-completion-at-point uses async by default
  ;; :bind (:irony-mode-map
  ;;        ([remap completion-at-point] . irony-completion-at-point-async)
  ;;        ([remap complete-symbol] . irony-completion-at-point-async))
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
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


;; indent tools
(use-package indent-tools
  :defer t
  :bind ("M-i" . indent-tools-hydra/body))


;;; Python
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq-default python-indent-offset 4)
  :config
  (setq gud-pdb-command-name "python3 -m pdb")
  (elpy-enable)
  ;; problem with ipython 5 prompt
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "--simple-prompt --pprint -i"))
(use-package elpy
  :defer t
  :commands (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  ;; prefer flycheck to flymake
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (elpy-use-ipython))


;;; Ruby
;; robe-mode: code navigation
;; inf-ruby:  repl integration
;; M-x inf-ruby (or C-c C-s) to start ruby process
;; then C-c C-l to load current ruby file
;; (add-hook 'ruby-mode-hook 'subword-mode)
;; (add-hook 'ruby-mode-hook 'eldoc-mode)
(use-package ruby-mode
  :defer t
  :bind (:ruby-mode-map
         ("C-M-p" . backward-list)
         ("C-M-n" . forward-list))
  ;; :init (inf-ruby-console-auto)
  :config
  (add-hook 'ruby-mode-hook 'eldoc-mode))
(use-package robe
  :defer t
  :init (add-hook 'ruby-mode-hook 'robe-mode)
  ;; company-robe contained within robe package
  :config (add-to-list 'company-backends 'company-robe))


;;; Common Lisp
;; M-x slime
;; slime handles indent correctly
;; (setq lisp-indent-function 'common-lisp-indent-function)
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'json-mode-hook 'rainbow-delimiters-mode))
(use-package paredit
  :defer t
  :init (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  ;; M-s : paredit-splice-sexp
  )
(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  ;; TODO : requires slime-company
  (setq slime-contribs '(slime-fancy slime-company)))
;; (slime-setup '(slime-fancy slime-company))

;;; Scheme
;; M-x run-geiser
(use-package geiser
  :defer t
  ;; :config (setq geiser-active-implementations '(racket))
  )


;;; OCaml
;; tuareg, merlin (minor mode)
;; install opam
(use-package tuareg
  ;; opam install tuareg
  :defer t)
(use-package merlin
  ;; opam install merlin
  :defer t
  :init (add-hook 'tuareg-mode-hook 'merlin-mode)
  ;; merlin-company added by merlin.el
  )
(use-package utop
  ;; opam install utop
  :defer t
  :config (setq utop-command "opam config exec -- utop -emacs"))


;;; Javascript & HTML & CSS

(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  ;; :init (add-hook 'js2-mode-hook 'subword-mode)
  :config
  (setq js-indent-level 2)
  ;; requires tern: npm install -g tern
  ;; add ~/.tern-project to get tern working
  (add-hook 'js2-mode-hook 'tern-mode)
  ;; company-tern
  (use-package company-tern
    :defer t
    :init (add-to-list 'company-backends 'company-tern))
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
  :init
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
            #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  :config
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
    (setq LaTeX-electric-left-right-brace t)))
;; LaTeX compile   C-c C-a
;; LaTeX math mode C-c ~
;; LaTeX insert environment C-c C-e
;; LaTeX insert item        C-c C-j


(defun my-latex-setup ()
    "To be set as mode hook for latex and org modes."
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands))
                        company-backends)))
(use-package company-math
  :defer t
  :init
  (add-hook 'org-mode-hook 'my-latex-setup)
  (add-hook 'TeX-mode-hook 'my-latex-setup)
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))


;;; yaml mode
(use-package yaml-mode
  :defer t
  :bind (:map yaml-mode-map
         ("C-m" . newline-and-indent)
         ("C-c C-d" . ansible-doc))
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
      ((and window-system
            (file-exists-p my-theme-dark)) ;by default use dark theme
       (message "Loading dark theme ...")
       (load-file my-theme-dark))
      (t
       (load-theme 'tango-dark t)))
(defun my-themes (_theme)
  "Command line switch placeholder.")
(add-to-list 'command-switch-alist '("dark" . my-themes))
(add-to-list 'command-switch-alist '("light" . my-themes))
(add-to-list 'command-switch-alist '("modern" . my-themes))


;; custom file load at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
