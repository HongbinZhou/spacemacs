
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
(global-set-key (kbd "C-x g") 'magit-status)

;; regexp count occurrences
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(global-set-key (kbd "C-c o") 'count-matches)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; disalbe better-defaults C-w key bindings
(global-set-key (kbd "C-w") 'kill-region)

;; easy-kill
(eval-after-load "easy-kill"
  (global-set-key [remap kill-ring-save] 'easy-kill))

