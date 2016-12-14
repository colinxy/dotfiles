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

(defun file-to-string (file)
  "Read all lines of FILE into string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; (setq initial-buffer-choice "~/TODO.org")

;; version control follow symbolic links
(setq vc-follow-symlinks t)

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

;; auto revert
(global-auto-revert-mode)

;; do not indent with tabs
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(global-set-key (kbd "RET") 'newline-and-indent)

;; some keys are easy to mispress
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-x C-w"))
;; C-w is only enabled when a region is selected
(defun my-kill-region ()
  "Cuts only when a region is selected."
  (interactive)
  (when mark-active
    (kill-region (region-beginning) (region-end))))
(global-set-key (kbd "C-w") 'my-kill-region)

;; (define-key esc-map "g" 'goto-line)

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
(global-set-key (kbd "M-\"") 'insert-pair) ; insert ""

;; upcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; http://cachestocaches.com/2015/8/getting-started-use-package/
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (require 'diminish)
;; (require 'bind-key)


;;--------------;;
;;;  speedbar  ;;;
;;--------------;;

;; (require 'speedbar)

(with-eval-after-load 'speedbar
  (add-to-list 'speedbar-frame-parameters '(width . 30))
  (setq speedbar-use-images nil)
  (setq speedbar-show-unknown-files t)
  (setq speedbar-initial-expansion-list-name "buffers")
  (setq speedbar-default-position 'left)
  (speedbar-add-supported-extension ".lisp") ;common lisp
  (speedbar-add-supported-extension ".ss")   ;scheme
  (speedbar-add-supported-extension ".ml"))  ;ocaml
(global-set-key (kbd "M-s M-s")
                'speedbar)


;;-------------;;
;;;   dired   ;;;
;;-------------;;

;; (require 'dired)
;; (setq delete-by-moving-to-trash t)

;; dired file search
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-s")
    'dired-isearch-filenames)
  (define-key dired-mode-map (kbd "C-M-s")
    'dired-isearch-filenames-regexp))

;; BSD ls does not support --dired
(when (not (eq system-type 'gnu/linux))
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(setq dired-listing-switches "-alh")


;; neotree
(when (eq system-type 'darwin)
  (setq neo-theme (if window-system 'icons 'arrow))
  (global-set-key (kbd "C-x C-d") 'neotree-toggle))


;;------------;;
;;; org-mode ;;;
;;------------;;

;; org-mode
(with-eval-after-load 'org
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; from *Help*, but not working
  ;; If you set this variable to the symbol `{}', the braces are
  ;; *required* in order to trigger interpretations as sub/superscript.
  (setq org-use-sub-superscripts '{})
  ;; org agenda
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c b") 'org-iswitchb))


;;--------------------------;;
;;; convenience and themes ;;;
;;--------------------------;;

;; enable interactively do things (ido)
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere t)

;; imenu
(setq imenu-auto-rescan 1)

;; pdf-tools binary (epdfinfo) installed from homebrew
;; use pdf-tools instead of doc-view
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

;; http://emacs.stackexchange.com/a/337/12003
(defun my-expose-global-bindng-in-mode-map (binding mode-map)
  "Expose global BINDING in MODE-MAP."
  (define-key mode-map binding
    (lookup-key (current-global-map) binding)))

;; exec-path-from-shell: consistent with shell in Mac OS X
(when (memq window-system '(mac ns))
  ;; (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; mutiple cursor
;; Shift key does not work for terminal
(when (require 'multiple-cursors nil t)
  ;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
  ;; (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)
  )

;; highlight TODO FIXME CHECKME (s) (make sure it is highlighted)
(when (require 'fic-mode nil t)
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "CHECKME"))
  (add-hook 'prog-mode-hook 'fic-mode))

;; emacs themes
(if window-system
    ;; gui
    (progn
      ;; theme for solarized
      ;; (setq custom-safe-themes t)
      (setq x-underline-at-descent-line t) ; modeline underline

      (with-eval-after-load 'solarized
        (setq solarized-high-contrast-mode-line t)
        (setq solarized-distinct-fringe-background t)
        (setq solarized-distinct-doc-face t)
        (setq solarized-use-more-italic t)
        (setq solarized-use-variable-pitch t)
        (setq solarized-emphasize-indicators t))
      (load-theme 'solarized-dark t))
  ;; terminal
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
(spaceline-toggle-flycheck-error)
(spaceline-toggle-flycheck-warning)


;; powerline

;; (when (require 'powerline nil t)
;;   ;; powerline color
;;   (powerline-default-theme)
;;   (setq powerline-default-separator 'wave))


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


;;; magit
(global-set-key (kbd "C-x g") 'magit-status)


;;------------;;
;;; Flycheck ;;;
;;------------;;

(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;;; programming language support

;; enable code folding
;; (add-hook 'prog-mode-hook #'hs-minor-mode)
;; gud (grand unified debugger)
;; (require 'gud)

(defun my-select-current-line ()
  "Handy function for selection current line."
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

;;---------------;;
;;;   company   ;;;
;;---------------;;

(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-irony
                                   company-irony-c-headers
                                   merlin-company-backend
                                   company-robe)))
(add-hook 'after-init-hook 'global-company-mode)


;;-----------------;;
;;; auto-complete ;;;
;;-----------------;;

;; requires popup
;; (ac-config-default)

;; disable auto-complete for python-mode
;; conflicts with elpy (which use company)
;; (defadvice auto-complete-mode (around disable-auto-complete-for-python)
;;   "Disable auto-complete for 'python-mode'."
;;   (unless (or (eq major-mode 'python-mode)
;;               (eq major-mode 'c-mode)
;;               (eq major-mode 'c++-mode))
;;     ad-do-it))
;; (ad-activate 'auto-complete-mode)


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

;; use company-mode for C/C++
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)

;; irony-mode
;; always use clang as compiler: brew install llvm --with-clang
;; install-server compilation flags:
;; cmake -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_INSTALL_PREFIX\=/Users/yxy/.emacs.d/irony/ /Users/yxy/.emacs.d/elpa/irony-20161106.830/server && cmake --build . --use-stderr --config Release --target install
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
(setq irony-additional-clang-options '("-std=c++11"))


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
(with-eval-after-load 'python
  (setq-default python-indent-offset 4)
  (setq gud-pdb-command-name "python3 -m pdb") ;grand unified debugger
  ;; elpy
  (when (require 'elpy nil t)
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-backend "jedi")
    (elpy-use-ipython)
    (elpy-enable))
  ;; problem with ipython 5 prompt
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "--simple-prompt --pprint -i"))


;; Ruby
;; robe-mode: code navigation
;; inf-ruby:  repl integration
;; M-x inf-ruby (or C-c C-s) to start ruby process
;; then C-c C-l to load current ruby file
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'subword-mode)
;; (add-hook 'ruby-mode-hook 'eldoc-mode)
(with-eval-after-load 'ruby
  (inf-ruby-console-auto))


;; Common Lisp
;; M-x slime
;; slime handles indent correctly
;; (setq lisp-indent-function 'common-lisp-indent-function)
(with-eval-after-load 'lisp-mode
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy)))

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

    ;; installed utop via opam
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t nil)
    (setq utop-command "opam config exec -- utop -emacs")
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)))


;; Javascript & HTML & CSS

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'subword-mode)
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
  ;; html entities
  (setq web-mode-enable-html-entities-fontification t)
  ;; highlight
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  ;; keybinding within current tag
  (define-key web-mode-map (kbd "M-n") 'web-mode-tag-next)
  (define-key web-mode-map (kbd "M-p") 'web-mode-tag-previous))


;; Markdown

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (require 'markdown-mode)
(setq markdown-command
      "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments")
;; (add-hook 'markdown-mode-hook 'flyspell-mode)


(provide '.emacs)
;;; .emacs ends here
