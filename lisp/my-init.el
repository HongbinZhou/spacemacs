
;;--------------------------------------------------
;; Global Settings
;;--------------------------------------------------

;; unbind Ctrl+z
(global-unset-key (kbd "C-x C-z"))

;; regexp count occurrences
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(global-set-key (kbd "C-c o") 'count-matches)

;; https://www.emacswiki.org/emacs/WhichFuncMode
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

;; setup files ending in ".tso" to open in nxml-mode
(use-package nxml-mode
  :mode "\\.tso\\'"
  ;; see: http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html
  ;; "<tab>" works in graphic user interface, while "TAB" works in terminal
  ;; :bind ("TAB" . noxml-fold-hide-show-element)
  :config
  (defun my-nxml-mode-hook ()
    (require 'noxml-fold)
    (noxml-fold-mode))
  (add-hook 'nxml-mode-hook 'my-nxml-mode-hook))

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
(use-package chinese-fonts-setup
  :config
  (setq cfs-profiles-directory (concat dotspacemacs-directory "chinese-fonts-setup/"))
  (setq cfs-use-system-type t)
  (setq cfs-profiles
        '("program" "org-mode" "read-book"))
  (setq cfs--current-profile "program")
  (setq cfs--profiles-steps (quote (("program" . 3))))

  ;; enable chinese-fonts-setup when start emacs
  (chinese-fonts-setup-enable)
  ;; make spacemacs mode-line Unicode show correctly
  (cfs-set-spacemacs-fallback-fonts))


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
  :init (setq markdown-command "pandoc -f markdown -t html --ascii --highlight-style pygments --mathjax"))


