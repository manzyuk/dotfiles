(require 'nnir)

;; The prefix [Google Mail] matches the default value of the variable
;; `gnus-ignored-newsgroups', making e.g. [Google Mail]/All Mail
;; invisible (http://www.emacswiki.org/emacs/GnusGmail).
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)
	       (nnimap-authinfo-file "~/.authinfo")
	       (nnir-search-engine imap)))

(setq user-full-name "Oleksandr Manzyuk")
(setq user-mail-address "manzyuk@gmail.com")
(setq send-mail-function 'smtpmail-send-it)

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-20,20f %* %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

(setq gnus-permanently-visible-groups
      "INBOX\\|hamilton\\|xmonad\\|bitt\\|nuim\\|All Mail")

(setq gnus-read-active-file 'some
      gnus-check-new-newsgroups 'ask-server)

(setq mm-discouraged-alternatives
      '("text/html"
	"text/richtext"))

(setq gnus-window-configuration
      '((summary        (1 4 0))
	(newsgroups     (1 1 3))
	(article        (1 1 3))))