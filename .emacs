;; disable start-up message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; delete trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; package archive
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


;; start up full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; mutiple cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)


;; emacs themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; theme for solarized
; (setq custom-safe-themes t)
; (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/solarized-theme-1.0.0")
; (load-theme 'solarized-dark t)
; (setq solarized-termcolors 256)
(load-theme 'tango-dark t)


;; smart mode line
(add-to-list 'load-path "~/.emacs.d/elpa/powerline-20151008.1449/")
(setq sml/shorten-directory t)
(setq sml/theme 'powerline)
(setq sml/no-confirm-load-theme t)
(setq sml/name-width 40)
(sml/setup)

;; powerline
;; (add-to-list 'load-path "~/.emacs.d/elpa/powerline/")
;; (require 'powerline)
;; (powerline-default-theme)
;; (set-face-attribute 'mode-line nil
;;                     :foreground "Black"
;;                     :background "DarkOrange"
;;                     :box nil)
;; (setq powerline-arrow-shape 'diagonal)
;; (setq-default mode-line-format '("%e"
;;      (:eval
;;       (concat
;;        (powerline-rmw 'left nil)
;;        (powerline-buffer-id 'left nil powerline-color1)
;;        (powerline-minor-modes 'left powerline-color1)
;;        (powerline-narrow 'left powerline-color1 powerline-color2)
;;        (powerline-vc 'center powerline-color2)
;;        (powerline-make-fill powerline-color2)
;;        (powerline-row 'right powerline-color1 powerline-color2)
;;        (powerline-make-text ":" powerline-color1)
;;        (powerline-column 'right powerline-color1)
;;        (powerline-percent 'right nil powerline-color1)
;;        (powerline-make-text "  " nil)))))


;; indent with 4 spaces
(setq-default c-basic-offset 4 c-default-style "k&r")
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)
(define-key global-map (kbd "RET") 'newline-and-indent)


;; show line number and column number
; (global-linum-mode 1)
(setq linum-relative-current-symbol "")
(setq column-number-mode t)
(show-paren-mode 1)


;; enable interactively do things (ido)
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)


;; enable auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)


;; enable YASnippet
;; using YASnippet 0.6.1 (legacy version)
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.6.1/yasnippet.el")
(require 'yasnippet)
; (require 'yasnippet-bundle)
;; set snippet directory
(setq yas/root-directory "~/.emacs.d/snippets/yasnippet-snippets")
(yas/load-directory yas/root-directory)
(yas/initialize)
(yas/global-mode 1)


;; auto-complete-c-header
(defun my:ac-c-header-init ()
   (require 'auto-complete-c-headers)
   (add-to-list 'ac-sources 'ac-source-c-headers)
   (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)


;; company-jedi for python auto-complete
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)


;; irony mode for c++
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))


;; markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; enable popup
;;; required by auto-complete
;(require 'popup)
;
;
;;; enable auto-complete-clang
;(require 'auto-complete-clang)
;;(global-set-key (kbd "C-`") 'ac-complete-clang)
;
;
;;; set default to split vertically
;(setq split-height-threshold nil)
;(setq split-width-threshold 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
