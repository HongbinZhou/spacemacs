
(global-set-key (kbd "C-2") 'set-mark-command)


;;; open txt and tso log with utf-8 coding
;;; ref: http://emacswiki.org/emacs/ChangingEncodings
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.tso\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.mlf\\'" 'utf-8)

;; setup files ending in “.tso” to open in nxml-mode
(add-to-list 'auto-mode-alist '("\\.tso\\'" . nxml-mode))

(global-set-key (kbd "C-z") 'eshell-here)
(global-set-key (kbd "C-c z") 'shell)

;;; unbind Ctrl+z
(global-unset-key (kbd "C-x C-z"))

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

;; regexp count occurrences
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(global-set-key (kbd "C-c o") 'count-matches)

;;; ref: https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))
