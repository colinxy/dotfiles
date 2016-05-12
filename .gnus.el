;;; package --- summary
;;; Commentary:

;; Emacs gnus client setup for gmail

;; References
;; http://www.mostlymaths.net/2010/12/emacs-30-day-challenge-using-gnus-to.html
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; https://github.com/kensanata/ggg#setting-up-gnus-for-gmail
;; https://github.com/brenns10/emacs/blob/master/gnus.org

;;; Code:

(setq user-mail-address	"colinxy@linux.ucla.edu"
      user-full-name	"Xinyu Colin Yang")

(require 'gnus)
(require 'starttls)
(require 'nnir)
(require 'smtpmail)

;; emacs compiled with gnutls support

;; linux.ucla.edu imap
(setq gnus-select-method
      '(nnimap "imap.gmail.com"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)))

;; (setq gnus-select-method '(nnimap "gmail"
;;                                   (nnimap-address "imap.gmail.com")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))

;; linux.ucla.edu smtp
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; gnus customization

(setq gnus-use-full-window nil)
(setq gnus-permanently-visible-groups "INBOX")
;; gnus always show messages, when if read
(setq gnus-parameters
      '((".*"
         (display . all)
         (gnus-use-scoring nil))))
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; tree view in summary buffer
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

;;; .gnus.el ends here
