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
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.places")
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode (quote right))
 '(scroll-preserve-screen-position 1)
 '(select-active-regions t)
 '(show-paren-mode t)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;; Disable the 3D highlighting of the mode line.
(set-face-attribute 'mode-line nil :box nil)

;;; Use y/n instead of yes/no in confirmation dialogs.
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
                 (mode . Custom-mode)
                 (mode . apropos-mode)
                 (mode . emacs-lisp-mode)
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")))
               ("LaTeX"
                (or
                 (mode . latex-mode)))
               ("Haskell"
                (or
                 (mode . haskell-mode)
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

;;; Fix colors in shell.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; Set environment variables.
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")
(setenv "PAGER" "cat")
(setenv "GEM_HOME" "/home/manzyuk/.gems")
(setenv "BIBINPUTS" "/home/manzyuk/texmf/bibtex/bib")

;;; `eshell' cannot handle `ssh' properly, fall back to `ansi-term' instead.
(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")))

;;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;;; text-mode hooks (log-edit-mode inherits these from text-mode).
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;;; Don't use spaces instead of tabs in makefiles.
(add-hook 'makefile-gmake-mode (lambda () (setq indent-tabs-mode t)))

;;; Enable command frequency tracking.
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;;; Enable reading/writing of comint input ring from/to a history file.
(defun add-to-process-sentinel (proc hook)
  "Add HOOK to the sentinel of PROC.  When PROC changes status,
first call HOOK, then call the original sentinel of PROC."
  (set-process-sentinel
   proc
   (lexical-let ((hook hook)
                 (sentinel
                  (or (process-sentinel proc)
                      #'default-sentinel)))
     (lambda (process event)
       (funcall hook)
       (funcall sentinel process event)))))

(defun default-sentinel (process event)
  "The default sentinel that inserts a message in the process's
buffer when the process status changes."
  (when (buffer-name (process-buffer process))
    (insert (format "\nProcess %s %s" process event))))

(defun turn-on-comint-input-ring ()
  "A hook that reads comint input ring from a history file when
the process buffer is created, and saves it to the file on each
process status change."
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format "~/.emacs.d/inferior-%s-history" (process-name process)))
      (comint-read-input-ring)
      (add-to-process-sentinel process 'comint-write-input-ring))))

;;; Haskell
(add-to-list 'load-path "~/.emacs.d/site-lisp/haskell-mode-2.8.0")
(load "haskell-site-file")

(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'inferior-haskell-mode-hook 'turn-on-comint-input-ring)

;;; HLint
(require 'hs-lint)

;;; Ruby
(autoload 'ruby-mode "ruby-mode" "Ruby mode." t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(setq ruby-program-name "irb --inf-ruby-mode")

(add-hook 'inferior-ruby-mode-hook 'turn-on-comint-input-ring)

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

;;; AUCTeX
(add-to-list 'load-path "~/.emacs.d/site-lisp/auctex-11.86")
(load "auctex.el" nil t t)

(require 'reftex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

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

(setq org-tags-column -80)

(setq org-capture-templates
      '(("t" "TODO" entry (file "~/org/todo.org") "* TODO %?")))

;;; Magit
(add-to-list 'load-path "~/.emacs.d/site-lisp/magit-0.8.2")
(require 'magit)
(global-set-key "\C-cg" 'magit-status)
