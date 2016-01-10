
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
(global-unset-key (kbd "C-z"))

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Magit rules!
(eval-after-load "magit"
  (global-set-key (kbd "C-x g") 'magit-status))

;; regexp count occurrences
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(global-set-key (kbd "C-c o") 'count-matches)

;; disalbe better-defaults C-w key bindings
(global-set-key (kbd "C-w") 'kill-region)

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
(global-set-key (kbd "M-i") 'helm-occur)

