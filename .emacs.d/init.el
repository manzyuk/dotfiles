;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PAGER"     "cat")
(setenv "EDITOR"    "emacsclient")
(setenv "VISUAL"    "emacsclient")
(setenv "GEM_HOME"  "/home/manzyuk/.gems")
(setenv "BIBINPUTS" "/home/manzyuk/texmf/bibtex/bib:.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-on-subword-mode () (subword-mode 1))

(defun regexp-alternatives (regexps)
  (mapconcat (lambda (regexp) (concat "\\(" regexp "\\)")) regexps "\\|"))

(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun reload-files ()
  (interactive)
  (mapc-buffers
   (lambda ()
     (let ((file-name (buffer-file-name)))
       (when file-name
         (kill-buffer)
         (find-file file-name))))))

;; Make `async-shell-command' more useful: supress (i.e., redirect
;; to /dev/null) all output and errors and disown the process.
(defun async-shell-command (command)
  (interactive
   (list
    (read-shell-command "Run: " nil nil
                        (and buffer-file-name
                             (file-relative-name buffer-file-name)))))
  (setq command (concat command " > /dev/null 2>&1 & disown"))
  (shell-command command))

(defun shell-command-to-list (command)
  (split-string (shell-command-to-string command) "\n" t))

(defun open-with-evince (name)
  (shell-command
   (format "evince \"$LIBRARY/%s\" > /dev/null 2>&1 & disown" name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Startup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't display the startup screen.
(setq inhibit-startup-screen t)

;; Don't display any message in *scratch* buffer at startup.
(setq initial-scratch-message nil)

;; Don't display buffer list when more than 2 files are loaded.
(setq inhibit-startup-buffer-menu t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable `column-number-mode'.
(setq column-number-mode t)

;; Don't show a cursor except in the selected window.
(setq-default cursor-in-non-selected-windows nil)

;; Visually indicate empty lines after the buffer end.
(setq-default indicate-empty-lines t)

;; Don't display menu bars.
(menu-bar-mode -1)

;; Softer colors.
(set-face-background 'fringe "gray20")
(set-face-background 'region "gray30")

;; Disable the 3D highlighting of the mode line.
(set-face-attribute 'mode-line nil :box nil)

;; For buffers visiting files show the full file name in the title
;; bar; for buffers not associated with files show the buffer name.
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Copy-paste ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable `delete-selection-mode'.
(delete-selection-mode 1)

;; Don't copy region to `kill-ring' by dragging mouse.
(setq mouse-drag-copy-region nil)

;; When I select something in another program to paste it into Emacs,
;; but kill something in Emacs before actually pasting it, don't lose
;; the selection; save it in the `kill-ring' before the Emacs kill so
;; that I can still paste it using C-y M-y.
(setq save-interprogram-paste-before-kill t)

;; Use clipboard, not primary selection for cutting and pasting.
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary nil)

;; Make mouse middle-click only paste from primary X11 selection, not
;; from clipboard or `kill-ring'.
(global-set-key [mouse-2] 'mouse-yank-primary)

;; Automatically make an active region the window selection.
(setq select-active-regions t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Comint ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comint)

;; Don't add input matching the last on the input ring.
(setq comint-input-ignoredups t)

;; Make the comint prompt read only.
(setq comint-prompt-read-only t)

;; Interpreter output moves point to the end of the output, but only
;; in the selected window so that we can simultaneously look at
;; previous outputs in other windows.
(setq-default comint-move-point-for-output 'this)

;; Input to interpreter causes the selected window to scroll.
(setq-default comint-scroll-to-bottom-on-input 'this)

;; Automatically close completions buffers in comint mode.
;; http://snarfed.org/automatically_close_completions_in_emacs_shell_comint_mode
(defun comint-close-completions ()
  "Close the comint completions buffer.

Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (when comint-dynamic-list-completions-config
    (set-window-configuration comint-dynamic-list-completions-config)
    (setq comint-dynamic-list-completions-config nil)))

(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (when ad-return-value (comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (when (member ad-return-value '(sole shortest partial))
    (comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
    (comint-close-completions)
    (when (not unread-command-events)
      ;; comint's "Type space to flush" swallows space.  Put it back in.
      (setq unread-command-events (listify-key-sequence " "))))

;; Deal with (some) non-SGR control sequences.

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

;; Enable reading/writing of comint input ring from/to a history file.
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
            (concat user-emacs-directory
                    (format "inferior-%s-history" (process-name process))))
      (comint-read-input-ring)
      (set-process-sentinel process #'comint-write-history-on-exit))))

;; If the buffer associated with a process is killed, the process's
;; sentinel is invoked when buffer-local variables  (in particular,
;; `comint-input-ring-file-name' and `comint-input-ring') are gone.
;; Therefore try to save the history every time a buffer is killed.
(add-hook 'kill-buffer-hook 'comint-write-input-ring)

;; Apparently, when Emacs is killed, `kill-buffer-hook' is not run
;; on individual buffers.  We circumvent that by adding a hook to
;; `kill-emacs-hook' that walks the list of all buffers and writes
;; the input ring (if it is available) of each buffer to a file.
(add-hook 'kill-emacs-hook (lambda () (mapc-buffers 'comint-write-input-ring)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define colors a la the default gnome-terminal color theme.
(setq ansi-color-black   "#555753"
      ansi-color-red     "#ef2929"
      ansi-color-green   "#8ae234"
      ansi-color-yellow  "#fce94f"
      ansi-color-blue    "#729fcf"
      ansi-color-magenta "#ad7fa8"
      ansi-color-cyan    "#34e2e2"
      ansi-color-white   "#eeeeec")

(setq ansi-color-names-vector
      (vector ansi-color-black
              ansi-color-red
              ansi-color-green
              ansi-color-yellow
              ansi-color-blue
              ansi-color-magenta
              ansi-color-cyan
              ansi-color-white))

(eval-after-load "ansi-color"
  ;; Copied from `ansi-color.el' and modified to support high intensity colors.
  '(defun ansi-color-make-color-map ()
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
       ansi-color-map)))

(defun ansi-color-generate-color-map ()
  (setq ansi-color-map (ansi-color-make-color-map)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-generate-color-map)

;; Use dirtrack-mode in shell buffers.
(add-hook 'shell-mode-hook (lambda () (dirtrack-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ido ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use flexible string matching with `ido-mode'.
(setq ido-enable-flex-matching t)

;; Always put . as the first item in file name lists; this allows the
;; current directory to be opened immediately with `dired'.
(setq ido-show-dot-for-dired t)

;; Enable `ido-mode'.
(ido-mode 1)

;; Enable `ido-everywhere'.
(ido-everywhere 1)

;; When I am opening a file (with `find-file' or `ido-find-file'), do
;; not suggest files generated by various programs.
(setq completion-ignored-extensions
      (nconc (list
              ;; Files generated by GHC
              ".hi"
              ;; Files generated by MIT Scheme
              ".bci" ".bin" ".com"
              ;; Files generated by LaTeX and friends
              ".dvi" ".pdf" ".ps"
              )
             completion-ignored-extensions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clean up whitespace in the buffer on save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Don't request confirmation before visiting a new file or buffer.
(setq confirm-nonexistent-file-or-buffer nil)

;; Enable `global-auto-revert-mode'.
(global-auto-revert-mode 1)

;; Enable `desktop-save-mode'.
(desktop-save-mode 1)

;; Enable `icomplete-mode'.
(icomplete-mode 1)

;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;; Don't make backup files.
(setq-default make-backup-files nil)

;; Always automatically save my place in any file.
(setq-default save-place t)
(require 'saveplace)

;; Enable `savehist-mode'.
(savehist-mode 1)

;; Scroll commands move point to always keep its screen position unchanged.
(setq scroll-preserve-screen-position 1)

;; Enable `show-paren' mode.
(show-paren-mode 1)

;; Default method to use for transferring files.
(setq tramp-default-method "ssh")

;; Uniquify buffer names with parts of directory name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Use y/n instead of yes/no in confirmation dialogs.
(fset 'yes-or-no-p 'y-or-n-p)

;;; Start server.
(server-start)

;;; Open URL links in Google Chrome.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Set the default coding system to UTF-8-UNIX.
(prefer-coding-system 'utf-8-unix)

;; Replace `dabbrev-expand' with `hippie-expand'.
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

;; Automatically make scripts starting with #! executable.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "\r" 'dired-find-alternate-file)))

;; `text-mode' hooks.
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Don't use spaces instead of tabs in makefiles.
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mail ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My full name and email.
(setq user-full-name    "Oleksandr Manzyuk")
(setq user-mail-address "manzyuk@gmail.com")

(require 'smtpmail)

;; Enable sending email from Emacs using my GMail account.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ibuffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace standard buffer menu with `ibuffer'.
(global-set-key "\C-x\C-b" 'ibuffer)

;; Define some filter groups.
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
                 (mode . latex-mode)
                 (mode . bibtex-mode)))
               ("Scheme"
                (or
                 (mode . scheme-mode)
                 (mode . inferior-scheme-mode)))
               ("Haskell"
                (or
                 (mode . haskell-mode)
                 (mode . literate-haskell-mode)
                 (mode . inferior-haskell-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Haskell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/haskell-mode-2.8.0")
(load "haskell-site-file")

(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-subword-mode)

(add-hook 'inferior-haskell-mode-hook 'turn-on-comint-history)

(set-default 'haskell-literate-default 'tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HLint ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'hs-lint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ruby ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'ruby-mode "ruby-mode" "Ruby mode." t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(setq ruby-program-name "irb --inf-ruby-mode")

(add-hook 'inferior-ruby-mode-hook 'turn-on-comint-history)

;; Add tab-completion to the inferior Ruby mode using `irb/completion'.
(defun inferior-ruby-completions (stub)
  "Return a list of completions for the line of Ruby code starting with STUB."
  (let* ((process (get-buffer-process ruby-buffer))
         (comint-filter (process-filter process))
         (kept "")
         completions)
    (set-process-filter
     process
     (lambda (proc string)
       (setf kept (concat kept string))))
    (process-send-string
     process
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
            (define-key inferior-ruby-mode-map "\t" 'inferior-ruby-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scheme-program-name "/home/manzyuk/bin/mit-scheme")
(add-hook 'inferior-scheme-mode-hook 'turn-on-comint-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ParEdit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'enable-paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(dolist (mode-hook
         '(emacs-lisp-mode-hook
           scheme-mode-hook
           inferior-scheme-mode-hook))
  (add-hook mode-hook 'enable-paredit-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AUCTeX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/auctex-11.86")
(load "auctex.el" nil t t)

;; Don't fontify subscript and superscript strings.
(setq font-latex-fontify-script nil)

;; Fotify sectioning macros with a color face only.
(setq font-latex-fontify-sectioning 'color)

;; Automatically save style information when saving the buffer.
(setq TeX-auto-save t)

;; Parse file after loading it if no style hook is found for it.
(setq TeX-parse-self t)

;; Don't ask for confirmation to save files before starting TeX.
(setq TeX-save-query nil)

;; Use source specials for forward and inverse search.
(setq TeX-source-correlate-method 'source-specials)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-source-correlate-mode 1)))

;; Start server for inverse search.
(setq TeX-source-correlate-start-server t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RefTeX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'reftex)

;; Turn RefTeX plug-ins on in LaTeX buffers.
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp")

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
       (let ((pos (max (1- (point)) (point-min))))
         (and (not (get-text-property pos 'keymap))
              (not (get-text-property pos 'org-no-flyspell))
              ;; don't check spelling inside code blocks and block delimiters
              (not (some (lambda (ovl)
                           (eql (overlay-get ovl 'face)
                                'org-block-background))
                         (overlays-at pos)))
              (not (org-mode-in-block-delimiter-p)))))))

(setq org-src-fontify-natively t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/magit-1.0.0")

(require 'magit)

(global-set-key "\C-cg" 'magit-status)

(set-face-foreground 'magit-diff-add ansi-color-green)
(set-face-foreground 'magit-diff-del ansi-color-red)
(set-face-foreground 'magit-diff-hunk-header ansi-color-cyan)
(set-face-background 'magit-item-highlight "gray25")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Anything ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq library-files
      `((name       . ,(getenv "LIBRARY"))
        (candidates . ,(shell-command-to-list "ls $LIBRARY"))
        (action     . (("Open with Evince" . open-with-evince)))))

(global-set-key "\C-cv" 'anything-library)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Google Translate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun google-translate (text)
  (interactive
   (list
    (read-from-minibuffer "Translate: ")))
  (with-output-to-temp-buffer "*Google Translate*"
    (set-buffer "*Google Translate*")
    (insert (format "%s" text))
    (facemenu-set-face 'bold (point-min) (point-max))
    (insert
     (format "\n\n%s"
             (shell-command-to-string (format "translate \"%s\"" text))))))

(global-set-key "\C-ct" 'google-translate)