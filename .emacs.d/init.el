;;; .emacs --- My emacs configuration

;;; Commentary:
;;
;; Flycheck made me do this
;;
;; install Emacs with cocoa on Mac OSX
;; $ brew install Emacs --with-cocoa --with-librsvg --with-gnutls --with-imagemagick
;;
;; TODO
;; 1. reorganize packages with use-package
;;
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

;; Easy PG setup
;; it used to work with emacs 24.5 and gnupg 1.x without any setup
;; now upgraded to emacs 25.1 and gnupg 2.1
(setq epa-pinentry-mode 'loopback)

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
(global-unset-key (kbd "C-x C-w"))
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
(if (not window-system)
    ;; terminal
    (menu-bar-mode -1)

  ;; gui
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
  ;; toggle-frame-fullscreen binded with <f11> (default)
  ;; (set-frame-parameter nil 'fullscreen 'fullboth) ; alternative
  ;; <f11> conflicts with mac command, bind it to M-<f11>
  (when (eq system-type 'darwin)
    (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)))

;; startup maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)

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


;;; before packages loads


;;-------------------;;
;;; package manager ;;;
;;-------------------;;

;; package archive
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
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(require 'use-package)
(require 'diminish)
(require 'bind-key)
;; (setq use-package-always-ensure t)

;; hide useless strings from modeline
(use-package abbrev
  :defer t
  :diminish abbrev-mode)

;; compile
(global-set-key (kbd "M-g M-c") 'compile)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; create a new frame for ediff
(add-hook 'ediff-before-setup-hook 'new-frame)
(add-hook 'ediff-quit-hook 'delete-frame)

;;-------------;;
;;;   dired   ;;;
;;-------------;;

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
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames)
              ("C-M-s" . dired-isearch-filenames-regexp)
              ("=" . my-dired-ediff-marked-files)
              ;; needs dired+
              ;; ("C-t C-t" . diredp-image-dired-display-thumbs-recursive)
              )
  :config
  (setq dired-listing-switches "-alh")
  ;; BSD ls does not support --dired
  (use-package ls-lisp
    :if (not (eq system-type 'gnu/linux))
    :config (setq ls-lisp-use-insert-directory-program nil))
  )


;; neotree
(use-package neotree
  :defer t
  :bind ("C-x C-d" . neotree-toggle)
  :config (setq neo-theme (if window-system 'icons 'arrow)))


;;--------------;;
;;; tramp-mode ;;;
;;--------------;;

;; tramp eshell: respect $PATH on remote host
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;;------------;;
;;; org-mode ;;;
;;------------;;

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
  (setq org-export-with-sub-superscripts '{})
  ;; (setq org-capture-templates '())
  )

;; C-c C-o: org-open-at-point


;;--------------------------;;
;;; convenience and themes ;;;
;;--------------------------;;

;; enable interactively do things (ido)
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere t)

;; maybe try ivy for a while ?
;; (ivy-mode 1)


;; imenu
(setq imenu-auto-rescan 1)

;; popup-imenu
(use-package popup-imenu
  :defer t
  :bind ("M-s M-i" . popup-imenu)
  :config (setq popup-imenu-style 'indent))

;; imenu-list
(use-package imenu-list
  :defer t
  :bind ("C-'" . imenu-list-smart-toggle)
  :config (setq imenu-list-focus-after-activation t))


;; undo tree
;; C-_  C-/  (`undo-tree-undo')
;; M-_  C-?  (`undo-tree-redo')
(use-package undo-tree
  :defer t
  :init (add-hook 'after-init-hook 'global-undo-tree-mode)
  :diminish undo-tree-mode)


;; pdf-tools binary (epdfinfo) installed from homebrew
;; use pdf-tools instead of doc-view
(setq doc-view-continuous t)
(setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
;; (pdf-tools-install)                     ;too slow, load on demand


;; shell integration
;; M-x eshell
;; M-x shell
;; M-x term
;; M-x ansi-term
;; (require 'term)
;; for term-mode, explicit shell name
;; (setq explicit-shell-file-name "/usr/local/bin/bash")
;; (require 'comint)
(add-hook 'term-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "M-(")
              (lambda ()
                (interactive)
                (term-send-raw-string "()")
                (term-send-left)))
            (define-key term-raw-map (kbd "M-\"")
              (lambda ()
                (interactive)
                (term-send-raw-string "\"\"")
                (term-send-left)))))
(global-set-key (kbd "M-t")
                (lambda () (interactive)
                  (ansi-term "/bin/bash")))


;; exec-path-from-shell: consistent with shell in Mac OS X
;; the current launcher em from .bashrc can let emacs inherit correct PATH
;; (when (memq window-system '(mac ns))
;;   ;; (require 'exec-path-from-shell)
;;   (exec-path-from-shell-initialize))


;; mutiple cursor
;; Shift key does not work for terminal
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)


;; highlight TODO FIXME CHECKME (s) (make sure it is highlighted)
(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "CHECKME"))
(add-hook 'prog-mode-hook 'fic-mode)


;; emacs themes
;; solarized theme by bbatsov
;; https://github.com/bbatsov/solarized-emacs
(use-package solarized
  :defer t
  :if window-system
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
;; when in terminal
(unless window-system
  (load-theme 'tango-dark t))

;;; modeline
;; mac specific, see https://github.com/milkypostman/powerline/issues/54
(when (eq system-type 'darwin)
  (setq ns-use-srgb-colorspace nil))
;; https://github.com/arranger1044/emacs.d/blob/master/rano/rano-customization.el
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

;; spaceline
;; depends on powerline
(require 'spaceline-config)
(setq powerline-default-separator (if window-system 'wave 'arrow))
(spaceline-emacs-theme)
(spaceline-toggle-flycheck-error-off)
(spaceline-toggle-flycheck-warning-off)


;;; YASnippet
;; let go of yasnippet for sometime
;; (when (require 'yasnippet nil t)
;;   ;; (require 'yasnippet-bundle)
;;   ;; set snippet directory
;;   (setq yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-snippets")
;;   ;; global
;;   ;; (yas-global-mode 1)
;;   ;; minor
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook #'yas-minor-mode))


;; async compilation of melpa package
(setq async-bytecomp-allowed-packages '(all))
(async-bytecomp-package-mode 1)


;;; magit
;; (global-set-key (kbd "C-x g") 'magit-status)
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

;; highlight changes
(use-package diff-hl
  :defer t
  :config
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))


;;---------------;;
;;;   company   ;;;
;;---------------;;

(use-package company
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (add-to-list 'company-backends '(company-irony
                                   company-irony-c-headers
                                   ;; merlin-company-backend
                                   company-robe
                                   ;; math
                                   company-math-symbols-unicode
                                   company-math-symbols-latex
                                   company-latex-commands
                                   ;; js
                                   company-tern
                                   ))
  ;; :diminish company-mode
  )


;; C/C++

;; treat .h as cpp header file
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; subword mode, treat CamelCase as 2 words
(add-hook 'c++-mode-hook 'subword-mode)
;; do not indent namespace
;; (c-set-offset 'innamespace 0)

;; indentation
(setq-default c-basic-offset 4
              c-default-style "k&r")

;; gdb
(setq gdb-many-windows t)

;; use company-mode for C/C++
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)

;; irony-mode
;; always use clang as compiler: brew install llvm --with-clang
;; install-server compilation flags:
;; cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_INSTALL_PREFIX\=/Users/yxy/.emacs.d/irony/ /Users/yxy/.emacs.d/elpa/irony-{latest}/server && cmake --build . --use-stderr --config Release --target install
;; requires libclang
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-eldoc)
(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)
            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; make irony aware of .clang_complete or cmake
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; C/C++ #if 0 comment
;; http://stackoverflow.com/q/4549015/5478848
(defun my-c-mode-font-lock-if0 (limit)
  "Show directive #if 0 as comment."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

;; (add-hook 'c-mode-common-hook
;;           #'(lambda ()
;;               (font-lock-add-keywords
;;                nil
;;                '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
;;                'add-to-end)))


;; Python
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (setq-default python-indent-offset 4)
  (setq gud-pdb-command-name "python3 -m pdb")
  :config
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
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (elpy-use-ipython))


;; Ruby
;; robe-mode: code navigation
;; inf-ruby:  repl integration
;; M-x inf-ruby (or C-c C-s) to start ruby process
;; then C-c C-l to load current ruby file
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'subword-mode)
;; (add-hook 'ruby-mode-hook 'eldoc-mode)
(with-eval-after-load 'ruby
  (define-key ruby-mode-map (kbd "C-M-p") 'backward-list)
  (define-key ruby-mode-map (kbd "C-M-n") 'forward-list)
  (inf-ruby-console-auto))


;; Common Lisp
;; M-x slime
;; slime handles indent correctly
;; (setq lisp-indent-function 'common-lisp-indent-function)
(with-eval-after-load 'lisp-mode
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-contribs '(slime-fancy))
  ;; (slime-setup '(slime-fancy))
  )

;; Scheme
;; M-x run-geiser
(setq geiser-active-implementations '(racket))


;; OCaml
;; tuareg, merlin (minor mode)
;; if only interactive, M-x run-ocaml
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; use tuareg
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (with-eval-after-load 'merlin-mode
      (add-to-list company-backends 'merlin-company-backend))

    ;; installed utop via opam
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t nil)
    (setq utop-command "opam config exec -- utop -emacs")
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)))


;; Javascript & HTML & CSS

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'subword-mode)
;; (add-hook 'js2-mode-hook 'jade-interaction-mode)
(with-eval-after-load 'js2-mode
  (setq js-indent-level 2))

;; edit HTML in web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook 'subword-mode)
  ;; indentation
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


;; Markdown

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-command
        "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments"))
;; (add-hook 'markdown-mode-hook 'flyspell-mode)


;; LaTeX
;; AUCTeX
(use-package tex
  :defer t
  ;; :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
            #'TeX-revert-document-buffer)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)                 ;pdflatex
  ;; math insert by pair $$, \(\)
  (setq TeX-electric-math '("$" . "$"))
  ;; insert {}, [], ()
  (setq LaTeX-electric-left-right-brace t)
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)
  )

;; LaTeX compile   C-c C-a
;; LaTeX math mode C-c ~
;; LaTeX insert environment C-c C-e
;; LaTeX insert item        C-c C-j


;; gnuplot mode
;; (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(use-package gnuplot-mode
  :defer t
  :mode "\\.gp\\'")


;; custom file load at last
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(provide '.emacs)
;;; .emacs ends here
