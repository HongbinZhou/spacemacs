
;;--------------------------------------------------
;; Global Settings
;;--------------------------------------------------

;; unbind Ctrl+z
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-\\"))

;; regexp count occurrences
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(global-set-key (kbd "C-c o") 'count-matches)

;; https://www.emacswiki.org/emacs/WhichFuncMode
;; [2017-12-05 Tue]: turn on which-function-mode, otherwise file name will be missing in mode-line
(which-function-mode 1)

;;--------------------------------------------------
;; Package Settings
;;--------------------------------------------------

;; simple
(use-package simple
  :bind ("C-2" . set-mark-command)
  :config
  ;; C-q [num in december]
  (setq read-quoted-char-radix 10))

;; mule
(use-package mule
  :config
  ;; open txt and tso log with utf-8 coding
  ;; ref: http://emacswiki.org/emacs/ChangingEncodings
  (modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
  (modify-coding-system-alist 'file "\\.tso\\'" 'utf-8)
  (modify-coding-system-alist 'file "\\.mlf\\'" 'utf-8))

;; align
;; Align your code in a pretty way.
(use-package align
  :bind ("C-x \\" . align-regexp))

;; Easy to find my-init.el
(use-package core-keybindings
  :config
  (defun spacemacs/find-my-initfile ()
    "Edit the `dotfile', in the current window."
    (interactive)
    (find-file-existing "~/.spacemacs.d/lisp/my-init.el"))
  (defun spacemacs/find-my-org-initfile ()
    "Edit the `dotfile', in the current window."
    (interactive)
    (find-file-existing "~/.spacemacs.d/lisp/my-org.el"))
  (spacemacs/set-leader-keys
    "fem" 'spacemacs/find-my-initfile)
  (spacemacs/set-leader-keys
    "feo" 'spacemacs/find-my-org-initfile))

;; ox-jive
(use-package ox-jive
  :load-path "~/.spacemacs.d/plugins")

;; mercurial
(use-package mercurial
  :load-path "~/.spacemacs.d/plugins")

;; setup files ending in ".tso" to open in web-mode
;; not to use nxml-mode as it always hang when do comment/uncomment...
(use-package web-mode
  :mode ("\\.tso\\'" "\\.hdr\\'" "\\.xml\\'"))

;; magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; easy-kill
(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

;; cc-mode
(use-package cc-mode
  :config
  (defun my-c-mode-common-hook ()
    (setq indent-tabs-mode nil)
    (setq c-default-style "bsd")
    (setq c-basic-offset 2)
    (setq tab-width 2)
    (electric-indent-mode 1)
    (c-toggle-auto-hungry-state 1)
    (line-number-mode t)
    (hs-minor-mode t)
    (fold-dwim-org/minor-mode t)

    ;; handle trailing space
    (setq-default show-trailing-whitespace t)
    (setq-default require-final-newline t)
    (setq-default delete-trailing-lines nil)
    ;; (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)
    ;; (add-hook 'local-write-file-hooks 'untabify-all)
    ;; (add-hook 'local-write-file-hooks 'indent-all)
    )
  (add-hook 'c-mode-common-hook
            'my-c-mode-common-hook))

;; helm-occur
(use-package helm
  :bind (("C-x C-f" . helm-find-files)
         ("M-i" . helm-occur)
         ("C-c M-i" . helm-imenu)))

;; org-agenda
(use-package org-agenda
  :bind (:map org-agenda-mode-map
              ("RET" . org-agenda-switch-to)))

;; recursive-narrow
(use-package recursive-narrow)

;; narrow-indirect
(use-package narrow-indirect
  :bind (:map ctl-x-4-map
              ("nd" . ni-narrow-to-defun-indirect-other-window)
              ("nn" . ni-narrow-to-region-indirect-other-window)
              ("np" . ni-narrow-to-page-indirect-other-window)))

;; org-src
(use-package org-src
  :bind (:map org-src-mode-map
              (;; nice key-bind for org src block!
               ;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
               "C-x C-s" . org-edit-src-exit)))

;; ruby-mode
(use-package ruby-mode
  :bind (:map ruby-mode-map
              ("C-c C-c" . ruby-add-breakpoint))
  :config
  (defun ruby-add-breakpoint ()
    "Add a break point"
    (interactive)
    (newline-and-indent)
    (insert "byebug")
    (highlight-lines-matching-regexp "^[ ]*byebug")))

;; cperl-mode
(use-package cperl-mode
  :mode "\\.pl\\'"
  :interpreter "perl"
  :bind (:map cperl-mode-map
              ("C-c C-c" . perl-on-buffer))
  :config
  ;; eval perl
  ;; ref: http://stackoverflow.com/questions/12408031/emacs-perl-mode-send-script-buffer-to-perl-interpreter
  (defun perl-on-buffer ()
    (interactive)
    (shell-command-on-region (point-min) (point-max) "perl" "*Perl Output*")
    (display-buffer "*Perl Output*"))

  (defun my-cperl-mode-hook ()
    (setq cperl-indent-level 2)
    (setq cperl-close-paren-offset -2)
    (setq cperl-continued-statement-offset 0) ; set 0 to prevent indent next line's {}
    (setq cperl-indent-parens-as-block nil)
    (setq cperl-tab-always-indent t)
    (setq indent-tabs-mode nil)
    (setq tab-width 2)
    (electric-indent-mode 1)
    (line-number-mode t)
    (hs-minor-mode t)
    (fold-dwim-org/minor-mode t)

    ;; handle trailing space
    (setq-default show-trailing-whitespace t)
    (setq-default require-final-newline t)
    (setq-default delete-trailing-lines nil)
    ;; (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)
    ;; (add-hook 'local-write-file-hooks 'untabify-all)
    ;; (add-hook 'local-write-file-hooks 'hbzhou/indent-all)
    )

  (add-hook 'cperl-mode-hook 'my-cperl-mode-hook))

;; haskell-mode
(use-package haskell-mode
  :config
  ;; Git rid of weird behavior in interactive-haskell repl
  ;; It should caused by the non-ascii "lambda" in the prompts,
  ;; so just replace use just "> "
  (setq haskell-interactive-prompt "> "))

;; javascript
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (defun my-js2-mode-hook ()
    (require 'js-comint)
    (setq inferior-js-program-command "node")
    (setenv "NODE_NO_READLINE" "1")
    (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
    (local-set-key (kbd "C-x C-r") 'js-send-region)
    (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
    (local-set-key (kbd "C-c b") 'js-send-buffer)
    (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)

    ;; ;; below is to use nodejs-repl instead
    ;; (require 'nodejs-repl)
    ;; (local-set-key (kbd "C-c C-e") 'nodejs-repl-send-last-sexp)
    ;; (local-set-key (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
    ;; (local-set-key (kbd "C-c C-c") 'nodejs-repl-send-buffer)

    )
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))



;;; set chinese font
(use-package cnfonts
  :config
  (setq cnfonts-directory (concat dotspacemacs-directory "cnfonts"))
  (setq cnfonts-use-system-type t)
  (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))
  (setq cnfonts--current-profile "program")

  ;; enable cnfonts when start emacs
  (cnfonts-enable)
  ;; make spacemacs mode-line Unicode show correctly
  (cnfonts-set-spacemacs-fallback-fonts))


(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc --from markdown --to html --standalone --mathjax --toc --number-sections")
  (let ((pandoc-css-file (concat dotspacemacs-directory "templates/pandoc.css")))
    (if (file-exists-p pandoc-css-file)
        (setq pandoc-css-opt (concat " --css " pandoc-css-file " --self-contained"))
      (setq pandoc-css-opt ""))
    (setq markdown-command (concat markdown-command pandoc-css-opt))))

;; eshell
(use-package eshell
  :init
  (setq eshell-aliases-file (concat dotspacemacs-directory "eshell/alias")))

;; winum-mode
(use-package winum
  :config
  (progn
    (setcdr (assoc 'winum-mode minor-mode-map-alist)
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-`") 'winum-select-window-by-number)
              (define-key map (kbd "C-²") 'winum-select-window-by-number)
              (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
              (define-key map (kbd "M-1") 'winum-select-window-1)
              (define-key map (kbd "M-2") 'winum-select-window-2)
              (define-key map (kbd "M-3") 'winum-select-window-3)
              (define-key map (kbd "M-4") 'winum-select-window-4)
              (define-key map (kbd "M-5") 'winum-select-window-5)
              (define-key map (kbd "M-6") 'winum-select-window-6)
              (define-key map (kbd "M-7") 'winum-select-window-7)
              (define-key map (kbd "M-8") 'winum-select-window-8)
              map))
    (setq winum-scope 'frame-local) ;change global to frame-local, https://github.com/deb0ch/emacs-winum
    (winum-mode)))

;; sox
(defun eshell/soxplay (filename)
  (interactive)
  (let* ((file-ext (file-name-extension filename))
         (audio-driver
          (cond
           ((string-equal system-type "windows-nt") ; Microsoft Windows
            "waveaudio 0")
           ((string-equal system-type "darwin")   ; Mac OS X
            (message "command not implemented yet on OS X"))
           ((string-equal system-type "gnu/linux") ; linux
            "alsa")))
         (sox-command
          (cond
           ((string-equal file-ext "wav")
            (format "sox %s -t %s" filename audio-driver))
           ((string-equal file-ext "pcm")
            (format "sox -t raw -r 22520 -b 16 -c 1 -e signed-integer %s -t %s" filename audio-driver))
           (t (message "ext %s not supported!" file-ext)))
          ))
         (insert sox-command)
         (eshell-send-input)))

;; --------------------------------------------------
;; replace tgt by src in the whole buffer
(defun string-replace (src tgt)
  (goto-char 1)
  (while (search-forward-regexp src nil t)
    (replace-match tgt t nil)))

;; add an start to each header for easy folding
(defun convert-riff-chunk-to-org ()
  (interactive)
  (org-mode)
  (string-replace "  Chunk" "* Chunk"))
;; --------------------------------------------------


;; evil

;; Switch to insert mode for the following modes
;; ref: https://github.com/syl20bnr/spacemacs/issues/4380
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;; justfile
(use-package makefile-mode
  :mode (("justfile" . makefile-mode)))

;; run shell command on current buffer file, for example:
;; (execute-shell-command-on-buffer tomlv) will validate current file if it's a
;; valid toml.
(defun execute-shell-command-on-buffer (shell-command-text)
  (interactive "MShell command:")
  (shell-command (format "%s %s" shell-command-text (shell-quote-argument buffer-file-name))))
