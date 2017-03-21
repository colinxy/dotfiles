
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
