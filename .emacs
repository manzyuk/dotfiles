(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-save-query nil)
 '(TeX-source-correlate-method (quote source-specials))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(ansi-color-names-vector ["#555753" "#ef2929" "#8ae234" "#fce94f" "#729fcf" "#ad7fa8" "#34e2e2" "#eeeeec"])
 '(before-save-hook (quote (whitespace-cleanup)))
 '(bookmark-default-file "~/.emacs.d/.bookmarks")
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(command-frequency-table-file "~/.emacs.d/frequencies")
 '(confirm-nonexistent-file-or-buffer nil)
 '(cursor-in-non-selected-windows nil)
 '(delete-selection-mode t)
 '(desktop-base-file-name ".desktop")
 '(desktop-base-lock-name ".desktop.lock")
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-save-mode t)
 '(font-latex-fontify-script nil)
 '(font-latex-fontify-sectioning (quote color))
 '(font-latex-script-display (quote (nil)))
 '(global-auto-revert-mode t)
 '(icomplete-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/.ido")
 '(ido-show-dot-for-dired t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-default-init t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(initial-scratch-message nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-drag-copy-region nil)
 '(reftex-plug-into-AUCTeX t)
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.places")
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode (quote right))
 '(scroll-preserve-screen-position 1)
 '(select-active-regions t)
 '(show-paren-mode t)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(user-mail-address "manzyuk@gmail.com")
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(fringe ((((class color) (background dark)) (:background "grey20"))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "#8ae234"))))
 '(magit-diff-del ((((class color) (background dark)) (:foreground "#ef2929"))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :foreground "#34e2e2"))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "gray25"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "grey30")))))

;;; Disable the 3D highlighting of the mode line.
(set-face-attribute 'mode-line nil :box nil)

;;; Use y/n instead of yes/no in confirmation dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;;; Start server.
(server-start)

;;; Replace standard buffer menu with `ibuffer'.
(global-set-key "\C-x\C-b" 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired"
                (mode . dired-mode))
               ("Emacs"
                (or
                 (mode . help-mode)
                 (mode . Info-mode)
                 (mode . shell-mode)
                 (mode . Custom-mode)
                 (mode . apropos-mode)
                 (mode . emacs-lisp-mode)
                 (mode . completion-list-mode)
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")))
               ("LaTeX"
                (or
                 (mode . latex-mode)))
               ("Haskell"
                (or
                 (mode . haskell-mode)
                 (mode . literate-haskell-mode)
                 (mode . inferior-haskell-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;; Make mouse middle-click only paste from primary X11 selection,
;;; not clipboard and kill ring.
(global-set-key [mouse-2] 'mouse-yank-primary)

;;; Set the default coding system to UTF-8-UNIX.
(prefer-coding-system 'utf-8-unix)

;;; More reasonable buffer naming for identically named files.
(require 'uniquify)

;;; Replace `dabbrev-expand' with `hippie-expand'.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))

(global-set-key "\M-/" 'hippie-expand)

;;; Automatically make scripts starting with #! executable.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Fix colors in shell.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun regexp-alternatives (regexps)
  (mapconcat (lambda (regexp) (concat "\\(" regexp "\\)")) regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '("\033\\[\\??[0-9;]*[^0-9;m]"   ; non-SGR CSI escape sequences
         "\033\\][0-2];.*?\007"         ; icon name escape sequences
         "\012\033\\[2K\033\\[1F"       ; noop
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker (or comint-last-output-start
                          (point-min-marker)))
        (end-marker (process-mark (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region start-marker end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

;;; Copied from `ansi-color.el' and modified to support high intensity colors.
(defun ansi-color-make-color-map ()
  "Creates a vector of face definitions and returns it.

The index into the vector is an ANSI code.  See the documentation of
`ansi-color-map' for an example.

The face definitions are based upon the variables
`ansi-color-faces-vector' and `ansi-color-names-vector'."
  (let ((ansi-color-map (make-vector 110 nil))
        (index 0))
    ;; miscellaneous attributes
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index e)
                 (setq index (1+ index)) ))
     ansi-color-faces-vector)
    ;; foreground attributes
    (setq index 30)
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index
                       (ansi-color-make-face 'foreground e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ;; background attributes
    (setq index 40)
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index
                       (ansi-color-make-face 'background e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ;; foreground attributes -- high intensity
    (setq index 90)
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index
                       (ansi-color-make-face 'foreground e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ;; background attributes -- high intensity
    (setq index 100)
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index
                       (ansi-color-make-face 'background e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ansi-color-map))

;;; Set environment variables.
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")
(setenv "PAGER" "cat")
(setenv "GEM_HOME" "/home/manzyuk/.gems")
(setenv "BIBINPUTS" "/home/manzyuk/texmf/bibtex/bib")

;;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;;; text-mode hooks (log-edit-mode inherits these from text-mode).
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Don't use spaces instead of tabs in makefiles.
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

;;; Enable reading/writing of comint input ring from/to a history file.
(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun turn-on-comint-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
                (format "~/.emacs.d/inferior-%s-history" (process-name process)))
      (comint-read-input-ring)
      (set-process-sentinel process #'comint-write-history-on-exit))))

;;; If the buffer associated with a process is killed, the process's
;;; sentinel is invoked when buffer-local variables  (in particular,
;;; `comint-input-ring-file-name' and `comint-input-ring') are gone.
;;; Therefore try to save the history every time a buffer is killed.
(add-hook 'kill-buffer-hook 'comint-write-input-ring)

(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))

;;; Apparently, when Emacs is killed, `kill-buffer-hook' is not run
;;; on individual buffers.  We circumvent that by adding a hook to
;;; `kill-emacs-hook' that walks the list of all buffers and writes
;;; the input ring (if it is available) of each buffer to a file.
(add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)

;;; Haskell
(add-to-list 'load-path "~/.emacs.d/site-lisp/haskell-mode-2.8.0")
(load "haskell-site-file")

(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'inferior-haskell-mode-hook 'turn-on-comint-history)

(set-default 'haskell-literate-default 'tex)

;;; HLint
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'hs-lint)

;;; Ruby
(autoload 'ruby-mode "ruby-mode" "Ruby mode." t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(setq ruby-program-name "irb --inf-ruby-mode")

(add-hook 'inferior-ruby-mode-hook 'turn-on-comint-history)

;;; Add tab-completion to the inferior Ruby mode using `irb/completion'.
(defun inferior-ruby-completions (stub)
  "Return a list of completions for the line of Ruby code starting with STUB."
  (let* ((process (get-buffer-process ruby-buffer))
         (comint-filter (process-filter process))
         (kept "")
         completions)
    (set-process-filter process
                        (lambda (proc string)
                          (setf kept (concat kept string))))
    (process-send-string process
                         (format "puts IRB::InputCompletor::CompletionProc.call('%s').compact\n" stub))
    (while (not (string-match inferior-ruby-prompt-pattern kept))
      (accept-process-output process))
    (when (string-match "^[[:alpha:]]+?Error: " kept)
      (error kept))
    (setf completions (butlast (split-string kept "[\r\n]") 2))
    (set-process-filter process comint-filter)
    completions))

(defun inferior-ruby-complete (&optional command)
  "Complete Ruby code at point."
  (interactive)
  (let* ((stub (thing-at-point 'line))
         (completions (inferior-ruby-completions stub)))
    (comint-dynamic-simple-complete stub completions)))

(add-hook 'inferior-ruby-mode-hook
          (lambda ()
            (define-key inferior-ruby-mode-map (kbd "TAB") 'inferior-ruby-complete)))

;;; Scheme
(setq scheme-program-name "/home/manzyuk/bin/mit-scheme")

;;; AUCTeX
(add-to-list 'load-path "~/.emacs.d/site-lisp/auctex-11.86")
(load "auctex.el" nil t t)

(require 'reftex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;; Org-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-7.3/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-7.3/contrib/lisp")

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-agenda-files '("~/org/todo.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-tag-alist
      '(("work"     . ?w)
        ("home"     . ?h)
        ("email"    . ?e)
        ("phone"    . ?p)
        ("computer" . ?c)))

(setq org-tags-column -90)

(setq org-capture-templates
      '(("t" "TODO" entry (file "~/org/todo.org") "* TODO %?")))

(eval-after-load "org"
  '(progn
     (defun org-mode-in-block-delimiter-p ()
       (save-excursion
         (beginning-of-line)
         (looking-at "^\s*#\\+\\(BEGIN\\|END\\)_.*$")))

     (defun org-mode-flyspell-verify ()
       (and (not (get-text-property (max (1- (point)) (point-min)) 'keymap))
            (not (get-text-property (max (1- (point)) (point-min)) 'org-no-flyspell))
            ;; don't check spelling inside code blocks and block delimiters
            (not (eql (get-text-property (max (1- (point)) (point-min)) 'face) 'org-block))
            (not (org-mode-in-block-delimiter-p))))))

;;; Magit
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit-0.8.2")

(require 'magit)

(global-set-key "\C-cg" 'magit-status)

;;; Enable sending email from Emacs using my GMail account.
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(require 'smtpmail)

;;; Anything
(add-to-list 'load-path "~/.emacs.d/site-lisp/anything-config")

(require 'anything-config)
(require 'anything-match-plugin)

(setq anything-mp-match-source-name nil)
(setq anything-mp-space-regexp "[\\][ ]")
(setq anything-candidate-number-limit nil)

(defun anything-library ()
  (interactive)
  (anything '(library-files)
            nil
            "View: "
            nil
            nil
            "*library*"))

(defun shell-command-to-list (command)
  (split-string (shell-command-to-string command) "\n" t))

(defun open-with-evince (name)
  (shell-command (format "evince \"$LIBRARY/%s\" > /dev/null 2>&1 & disown" name)))

(setq library-files
      `((name       . ,(getenv "LIBRARY"))
        (candidates . ,(shell-command-to-list "ls $LIBRARY"))
        (action     . (("Open with Evince" . open-with-evince)))))

(global-set-key "\C-cv" 'anything-library)

;;; Make `async-shell-command' more useful: supress (redirect to
;;; /dev/null) all output and errors and disown the process.
(defun async-shell-command (command)
  (interactive
   (list
    (read-shell-command "Run: " nil nil
                        (and buffer-file-name
                             (file-relative-name buffer-file-name)))))
  (setq command (concat command " > /dev/null 2>&1 & disown"))
  (shell-command command))

;;; Minimalistic Emacs interface to Google Translate.
(defun google-translate (text)
  (interactive
   (list
    (read-from-minibuffer "Translate: ")))
  (with-output-to-temp-buffer "*Google Translate*"
    (set-buffer "*Google Translate*")
    (insert (format "%s" text))
    (facemenu-set-face 'bold (point-min) (point-max))
    (insert (format "\n\n%s" (shell-command-to-string (format "translate \"%s\"" text))))))

(global-set-key "\C-ct" 'google-translate)
