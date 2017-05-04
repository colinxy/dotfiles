
(defun my-file-to-string (file)
  "Read all lines of FILE into string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


;; http://emacs.stackexchange.com/a/337/12003
(defun my-expose-global-bindng-in-mode-map (binding mode-map)
  "Expose global BINDING in MODE-MAP."
  (define-key mode-map binding
    (lookup-key (current-global-map) binding)))


(defun my-select-current-line ()
  "Handy function for selection current line."
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))


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


;; ocaml, opam
(eval-and-compile
  (defun my-opam-share-path ()
    (let ((opam-share
           (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
        (expand-file-name "emacs/site-lisp" opam-share)))))
;; (use-package tuareg
;;   :defer t
;;   :load-path (lambda () (my-opam-share-path)))
