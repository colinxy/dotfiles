;;; .emacs --- My emacs configuration

;;; Commentary:
;;
;; Flycheck made me do this
;;
;; install Emacs with cocoa on Mac OSX
;; $ brew install Emacs --with-cocoa
;;
;; TODO
;; 1. reorganize packages with use-package
;; 2. restructure the entire init-file with org-babel
;;
;;; Code:


;;----------------;;
;;; basic config ;;;
;;----------------;;

;; disable start-up message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message "colinxy")

;; version control follow symbolic links
(setq vc-follow-symlinks t)

;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; substitute y-or-n-p with yes-or-no-p
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

;; indentation support, do not indent with tabs
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; some keys are easy to mispress
;; (global-unset-key (kbd "C-m"))
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-x C-w"))
;; C-w is only enabled when a region is selected
(defun my-kill-region ()
  "Cuts only when a region is selected."
  (interactive)
  (when mark-active
    (kill-region (region-beginning) (region-end))))
(global-set-key (kbd "C-w") 'my-kill-region)

;; show line number and column number
;; (global-linum-mode 1)  ; show line number of the left
(setq column-number-mode t)
(show-paren-mode 1)
(when window-system
  (global-hl-line-mode))

;; consider CamelCase to be 2 words
(subword-mode)

;; auto insert pair
;; M-( ; insert ()
;; (global-set-key (kbd "M-(") 'insert-pair)
;; (setq parens-require-spaces nil)
(global-set-key (kbd "M-[") 'insert-pair)  ; insert []
(global-set-key (kbd "M-\"") 'insert-pair) ; insert ""

;; delete trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; package archive
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; reference
;; http://cachestocaches.com/2015/8/getting-started-use-package/
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (require 'diminish)
;; (require 'bind-key)


;;----------------------;;
;;; windows and moving ;;;
;;----------------------;;

;; for gui window
(when window-system
  (setq frame-title-format "%b")
  ;; (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; set font
  (set-face-attribute 'default nil
                      :font "Monaco 14")

  ;; for Mac OS X >= 10.7
  ;; toggle-frame-maximized binded with M-<f10>
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; toggle-frame-fullscreen binded with <f11> (default)
  ;; (set-frame-parameter nil 'fullscreen 'fullboth) ; alternative
  ;; <f11> conflicts with mac command, bind it to M-<f11>
  (when (eq system-type 'darwin)
    (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen))
  )


;; window management

(defun my-split-window-right-open-file ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (ido-find-file))
(defun my-split-window-right-switch-buffer ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (ido-switch-buffer))
;; C-x 3   to split right
;; C-x C-3 to split right and open file
(if window-system
    ;; gui
    (global-set-key (kbd "C-x C-3 f") 'my-split-window-right-open-file)
  ;; terminal
  (global-set-key (kbd "C-x M-3 f") 'my-split-window-right-open-file))
(if window-system
    ;; gui
    (global-set-key (kbd "C-x C-3 b") 'my-split-window-right-switch-buffer)
  ;; terminal
  (global-set-key (kbd "C-x M-3 b") 'my-split-window-right-switch-buffer))

(defun my-split-window-below-open-file ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (ido-find-file))
(defun my-split-window-below-switch-buffer ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (ido-switch-buffer))
;; C-x 2   to split below
;; C-x C-2 to split below and open file
(if window-system
    ;; gui
    (global-set-key (kbd "C-x C-2 f") 'my-split-window-below-open-file)
  ;; terminal
  (global-set-key (kbd "C-x M-2 f") 'my-split-window-below-open-file))
(if window-system
    ;; gui
    (global-set-key (kbd "C-x C-2 b") 'my-split-window-below-switch-buffer)
  ;; terminal
  (global-set-key (kbd "C-x M-2 b") 'my-split-window-below-switch-buffer))

;; move between windows
;; only works for gui
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super))
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)


;; mutiple cursor
;; Shift key does not work for terminal
;; load path handled by package.el
(require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)


;;--------------;;
;;; dired-mode ;;;
;;--------------;;

(setq delete-by-moving-to-trash t)

;; dired file search
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "C-s")
       'dired-isearch-filenames)
     (define-key dired-mode-map (kbd "C-M-s")
       'dired-isearch-filenames-regexp)))

;; BSD ls does not support --dired
(when (not (eq system-type 'gnu/linux))
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(setq dired-listing-switches "-alh")

;; (require 'dired+)


;;------------;;
;;; org-mode ;;;
;;------------;;

;; org-mode
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)


;;--------------------------;;
;;; themes and convenience ;;;
;;--------------------------;;

;; for continuous scroll in pdf
(setq doc-view-continuous t)

;; highlight TODO FIXME CHECKME (s) (make sure it is highlighted)
;; load path handled by package.el
(require 'fic-mode)
(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "CHECKME"))
(add-hook 'prog-mode-hook 'fic-mode)

;; shell integration
;; M-x eshell
;; M-x shell
;; M-x term
;; M-x ansi-term
(require 'comint)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; exec-path-from-shell: consistent with shell in Mac OS X
(when (memq window-system '(mac ns))
  (progn
    ;; load path handled by package.el
    (exec-path-from-shell-initialize))
  )

;; emacs themes
(if window-system
    ;; gui
    (progn
      ;; theme for solarized
      ;; (setq custom-safe-themes t)
      ;; load theme handled by package.el
      ;; (add-to-list 'custom-theme-load-path
      ;;              "~/.emacs.d/elpa/solarized-theme-20160106.15/")
      (setq x-underline-at-descent-line t) ; modeline underline
      (setq solarized-high-contrast-mode-line t)
      (setq solarized-distinct-fringe-background t)
      (setq solarized-distinct-doc-face t)
      (setq solarized-use-more-italic t)
      (setq solarized-use-variable-pitch t)
      (setq solarized-emphasize-indicators t)

      (load-theme 'solarized-dark t)
      )
  ;; terminal
  (load-theme 'tango-dark t))

;;; modeline

;; powerline
;; load path handled by package.el
;; mac specific, see https://github.com/milkypostman/powerline/issues/54
(setq ns-use-srgb-colorspace nil)
;; powerline color stolen from
;; https://github.com/arranger1044/emacs.d/blob/master/rano/rano-customization.el
(require 'powerline)
(set-face-attribute 'mode-line nil
                    :underline nil
                    :overline nil
                    :foreground "#fdf6e3"
                    :background "#2aa198"
                    :box nil
                    )
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fdf6e3")
;; (setq mode-line-in-non-selected-windows nil) ;do not use mode-line-inactive
(powerline-default-theme)
(setq powerline-default-separator 'wave)

;; smart mode line
;; (setq sml/shorten-directory t)
;; (setq sml/shorten-modes t)
;; (setq sml/theme 'powerline)
;; (setq sml/no-confirm-load-theme t)
;; (setq sml/name-width 40)  ; path-length
;; (sml/setup)


;; enable interactively do things (ido)
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)


;; enable YASnippet
;; laod path handled by package.el
;; (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20160131.948")
(require 'yasnippet)
;; (require 'yasnippet-bundle)
;; set snippet directory
(setq yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-snippets")
;; global
;; (yas-global-mode 1)
;; minor
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;;------------;;
;;; Flycheck ;;;
;;------------;;

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-gcc-language-standard "c++11")))

;; (require 'flycheck-color-mode-line)
;; (eval-after-load "flycheck"
;;   '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;;----------------;;
;;; company-mode ;;;
;;----------------;;

;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)
;; ;; customize company color
;; (require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,
;;                                    (color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


;; irony for C/C++
;; (require 'irony)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; company irony backend for c++
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; company-jedi for python auto-complete
;; (defun my-python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'my-python-mode-hook)


;;-----------------;;
;;; auto-complete ;;;
;;-----------------;;

;; requires popup
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; auto-complete-c-header
(defun my-ac-c-header-init ()
   (require 'auto-complete-c-headers)
   (add-to-list 'ac-sources
                'ac-source-c-headers)
   (add-to-list 'achead:include-directories
                '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"))
(add-hook 'c++-mode-hook 'my-ac-c-header-init)
(add-hook 'c-mode-hook 'my-ac-c-header-init)

;; enable auto-complete-clang
;; load path handled by package.el
(require 'auto-complete-clang)

;; make yasnippet work with auto-complete
;; prefer yasnippet to auto-complete
;; set the trigger key so that it can work together with yasnippet on tab key
;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; activate, otherwise, auto-complete will
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")


;;; programming language support

;; enable code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; general
(defun my-select-current-line ()
  "Handy function for selection current line."
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

;; C/C++

;; treat .h as cpp header file
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; indentation
(require 'cc-mode)
(setq-default c-basic-offset 4
              c-default-style "k&r")
;; (add-hook 'c-mode-hook (lambda () (setq comment-start "/* "
;;                                         comment-end   " */")))
;; (add-hook 'c++-mode-hook (lambda () (setq comment-start "/* "
;;                                           comment-end   " */")))

;; comment or uncomment line
(defun my-comment-or-uncomment-line-or-region ()
  "Comment or uncomment current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end))
  ;; (next-line)
  )
;; override default binding for C-c C-c
(eval-after-load 'c-mode
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c")
                             #'my-comment-or-uncomment-line-or-region)))
  )
(eval-after-load 'c++-mode
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-c")
                             #'my-comment-or-uncomment-line-or-region))))

;; Python

;; elpy and autopep8
(require 'py-autopep8)
(eval-after-load 'python-mode
  (progn
    (setq parens-require-spaces nil)
    ;; autopep8
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
    ;; elpy
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-backend "jedi")
    (elpy-enable)
    (elpy-use-ipython)))


;; javascript

;; indentation level
(setq js-indent-level 2)


;; Markdown

;; markdown mode
(add-to-list 'load-path "~/.emacs.d/elpa/markdown-mode-20160121.528/")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing markdown files" t)
(setq markdown-command
      "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments")
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-hook 'markdown-mode-hook 'flyspell-mode)


;;----------------------;;
;;; emacs code browser ;;;
;;----------------------;;

(add-to-list 'load-path "~/.emacs.d/elpa/ecb")
(require 'ecb)
;; do not autoload
;; (require 'ecb-autoloads)
(setq ecb-layout-name "left6")
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-show-tip-of-the-day 0)
(setq ecb-tip-of-the-day nil)
;; adjust layout left/right
;; adjust left column: sources/methods/history
(setq ecb-layout-window-sizes
      (quote (("left6"
               (ecb-sources-buffer-name 0.15 . 0.5)
               (ecb-history-buffer-name 0.15 . 0.1)
               (ecb-methods-buffer-name 0.15 . 0.4)))))
(setq ecb-auto-update-methods-after-save t)
;; navigating
;; [C-c . g h] : Go to history
;; [C-c . g m] : Go to methods
;; [C-c . g s] : Go to sources:
;; [C-c . g d] : Go to directories
;; [C-c . g 1] : Main buffer



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(ecb-options-version "2.50")
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(vc-annotate-background "#93a1a1")
 '(vc-annotate-color-map
   (quote
    ((20 . "#990A1B")
     (40 . "#FF6E64")
     (60 . "#cb4b16")
     (80 . "#7B6000")
     (100 . "#b58900")
     (120 . "#DEB542")
     (140 . "#546E00")
     (160 . "#859900")
     (180 . "#B4C342")
     (200 . "#3F4D91")
     (220 . "#6c71c4")
     (240 . "#9EA0E5")
     (260 . "#2aa198")
     (280 . "#69CABF")
     (300 . "#00629D")
     (320 . "#268bd2")
     (340 . "#69B7F0")
     (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#93115C"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#005369"))))
 '(company-scrollbar-fg ((t (:background "#003f4f"))))
 '(company-tooltip ((t (:inherit default :background "#003340"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

(provide '.emacs)
;;; .emacs ends here
