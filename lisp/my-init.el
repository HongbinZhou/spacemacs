
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for those packages which don't included in elpa
(add-to-list 'load-path "~/.spacemacs.d/plugins")
(require 'mercurial)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(global-set-key (kbd "C-2") 'set-mark-command)


;;; open txt and tso log with utf-8 coding
;;; ref: http://emacswiki.org/emacs/ChangingEncodings
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.tso\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.mlf\\'" 'utf-8)

;; setup files ending in “.tso” to open in nxml-mode
(add-to-list 'auto-mode-alist '("\\.tso\\'" . nxml-mode))

;;; unbind Ctrl+z
(global-unset-key (kbd "C-x C-z"))

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Magit rules!
(eval-after-load "magit"
  (global-set-key (kbd "C-x g") 'magit-status))

;; regexp count occurrences
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(global-set-key (kbd "C-c o") 'count-matches)

;; easy-kill
(eval-after-load "easy-kill"
  (global-set-key [remap kill-ring-save] 'easy-kill))

;;; C-q [num in december]
(setq read-quoted-char-radix 10)

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
          'my-c-mode-common-hook)

;; let M-e to go end of line
(define-key evil-motion-state-map (kbd "C-e") 'move-end-of-line)

;; let M-. do helm-gtags-dwin
(define-key evil-normal-state-map (kbd "M-.") 'helm-gtags-dwim)

;; helm-occur
(with-eval-after-load 'helm
  ;; tmp work around for issue: https://github.com/syl20bnr/spacemacs/issues/4538
  (setq helm-echo-input-in-header-line nil)

  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-i") 'helm-occur))

;; org-agenda
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map
    (kbd "RET") 'org-agenda-switch-to))

;; added keybindings
(spacemacs/set-leader-keys
  "br" 'rename-buffer)


;; recursive-narrow
(use-package recursive-narrow)

;; narrow-indirect
(use-package narrow-indirect
  :init
  (bind-key "nd" 'ni-narrow-to-defun-indirect-other-window   ctl-x-4-map)
  (bind-key "nn" 'ni-narrow-to-region-indirect-other-window  ctl-x-4-map)
  (bind-key "np" 'ni-narrow-to-page-indirect-other-window    ctl-x-4-map))

;; Easy to find my-init.el
(defun spacemacs/find-my-initfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing "~/.spacemacs.d/lisp/my-init.el"))

(spacemacs/set-leader-keys
  "fem" 'spacemacs/find-my-initfile)

;; nice key-bind for org src block!
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

;; python breakpoint key
(define-key python-mode-map (kbd "C-c C-b") 'python-toggle-breakpoint)
