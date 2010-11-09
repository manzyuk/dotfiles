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
 '(icomplete-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/.ido")
 '(ido-show-dot-for-dired t)
 '(indent-tabs-mode nil)
 '(inhibit-default-init t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(initial-scratch-message nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(reftex-plug-into-AUCTeX t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/.places")
 '(savehist-mode t nil (savehist))
 '(scroll-preserve-screen-position 1)
 '(show-paren-mode t)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;; Add ~/.emacs.d/site-lisp/ and all its subdirectories to load path.
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

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
               ("SLIME"
                (or
                 (mode . lisp-mode)
                 (name . "^\\*slime")
                 (name . "^\\*inferior-lisp\\*$")))
               ("Haskell"
                (or
                 (mode . haskell-mode)
                 (mode . inferior-haskell-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;; Enable clipboard.
(setq interprogram-cut-function   'x-select-text
      interprogram-paste-function 'x-cut-buffer-or-selection-value)

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

;;; `eshell' cannot handle `ssh' properly, fall back to `ansi-term' instead.
(add-hook 'eshell-first-time-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")))

;;; Enable `dired-find-alternate-file'.
(put 'dired-find-alternate-file 'disabled nil)

;;; text-mode hooks (log-edit-mode inherits these from text-mode).
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;;; Haskell
(load "~/.emacs.d/site-lisp/haskell-mode-2.8.0/haskell-site-file")

(autoload 'haskell-mode "haskell-mode" "Haskell mode." t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;; AUCTeX
(load "auctex.el" nil t t)

(require 'reftex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
