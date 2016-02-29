
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  basic config  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable start-up message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; follow symbolic links
(setq vc-follow-symlinks t)

;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; substitute y-or-n-p with yes-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; smooth scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
;; (load-file "~/.emacs.d/elpa/smooth-scrolling/smooth-scrolling.el")
;; (require 'smooth-scrolling)

;; delete trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enable code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; highlight TODO FIXME (s) (make sure it is highlighted)
(add-to-list 'load-path "~/.emacs.d/elpa/fic-mode-20140421.922/")
(require 'fic-mode)
(add-hook 'prog-mode-hook `turn-on-fic-mode)

;; indentation support, do not indent with tabs
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-unset-key (kbd "C-m"))
(global-unset-key (kbd "C-o"))

;; show line number and column number
;; (global-linum-mode 1)
;; personal costumized internally, changed current line indicator
;; (add-to-list 'load-path "~/.emacs.d/elpa/linum-relative-20160117.2200/")
;; (require 'linum-relative)
;; (linum-relative-on)
(setq column-number-mode t)
(show-paren-mode 1)
(when window-system
  (add-hook 'prog-mode-hook 'hl-line-mode))

;; auto insert pair
;; M-( ; insert ()
(global-set-key (kbd "M-[") 'insert-pair)  ; insert []
;; (global-set-key (kbd "M-{") 'insert-pair)  ; insert {}  ; conflict keybinding
(global-set-key (kbd "M-\"") 'insert-pair) ; insert ""

;; package archive
(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;; melpa stable
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;  windows and moving  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for gui window
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; The value is in 1/10pt, so 100 will give you 10pt
  (set-face-attribute 'default nil :height 140)

  ;; for Mac OS X >= 10.7
  ;; toggle-frame-maximized binded with M-<f10>
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized)) ; alternative
  ;; toggle-frame-fullscreen binded with <f11> (default)
  ;; (set-frame-parameter nil 'fullscreen 'fullboth) ; alternative
  (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen))


;; window management

(defun my-split-window-right-open-file ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (ido-find-file)
  )
(defun my-split-window-right-switch-buffer ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (ido-switch-buffer)
  )
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
  (ido-find-file)
  )
(defun my-split-window-below-switch-buffer ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (ido-switch-buffer)
  )
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
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)


;; mutiple cursor
;; Shift key does not work for terminal
(add-to-list 'load-path "~/.emacs.d/elpa/multiple-cursors-1.3.0/")
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)


;;;;;;;;;;;;;;;;;;;;;;;;;  themes and convenience  ;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs themes
(if window-system
    ;; gui
    (progn
      ;; theme for solarized
      ;; (setq custom-safe-themes t)
      ;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
      (add-to-list 'custom-theme-load-path
                   "~/.emacs.d/elpa/solarized-theme-20160106.15/")
      (load-theme 'solarized-dark t)
      (setq x-underline-at-descent-line t)
      ;; (setq solarized-termcolors 256)
      )
  ;; terminal
  (load-theme 'tango-dark t))


;; shell integration
;; M-x eshell
;; M-x shell
;; M-x term
;; M-x ansi-term
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;; modeline

;; smart mode line
;; (setq sml/shorten-directory t)
;; (setq sml/shorten-modes t)
;; (setq sml/theme 'powerline)
;; (setq sml/no-confirm-load-theme t)
;; (setq sml/name-width 40)  ; path-length
;; (sml/setup)

;; powerline
(add-to-list 'load-path "~/.emacs.d/elpa/powerline/")
;; see https://github.com/milkypostman/powerline/issues/54
(setq ns-use-srgb-colorspace nil)
(require 'powerline)
(powerline-default-theme)


;; enable interactively do things (ido)
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)


;; enable YASnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20160131.948")
(require 'yasnippet)
;; (require 'yasnippet-bundle)
;; set snippet directory
(setq yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-snippets")
;; global
(yas-global-mode 1)
;; minor
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  company-mode  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/elpa/company-20160228.1509")
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
;; (add-to-list 'load-path "~/.emacs.d/elpa/irony-20160203.1207/")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  auto-complete  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-clang-20140409.52/")
(require 'auto-complete-clang)

;; make yasnippet work with auto-complete
;; prefer yasnippet to auto-complete
;; set the trigger key so that it can work together with yasnippet on tab key
;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; activate, otherwise, auto-complete will
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")


;;; language support

;; general
(defun my-select-current-line ()
  "handy function for selection current line"
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
(setq-default c-basic-offset 4 c-default-style "k&r")
;; (add-hook 'c-mode-hook (lambda () (setq comment-start "/* "
;;                                         comment-end   " */")))
;; (add-hook 'c++-mode-hook (lambda () (setq comment-start "/* "
;;                                           comment-end   " */")))

;; comment or uncomment line
(defun my-comment-or-uncomment-line-or-region ()
  "comment or uncomment current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end))
  ;; (next-line)
  )
;; override default binding for C-c C-c
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c")
                           #'my-comment-or-uncomment-line-or-region)))
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c")
                           #'my-comment-or-uncomment-line-or-region)))

;; Python

;; elpy
(elpy-enable)

;; autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  emacs code browser  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/elpa/ecb")
(require 'ecb)
;(require 'ecb-autoloads)
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
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default)))
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
 '(powerline-default-separator (quote wave))
 '(solarized-distinct-fringe-background nil)
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
