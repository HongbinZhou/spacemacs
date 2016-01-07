(setenv "PATH"
	(concat
	 "C:/Python27" ";"
	 "C:/cygwin64/bin" ";"
	 "C:/cygwin64/usr/sbin" ";"
	 (getenv "PATH")
	 ))

(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")
(setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe")

(global-set-key (kbd "C-x C-c") 'my-done)
(defun my-done ()
  (interactive)
  (server-edit)
  (make-frame-invisible nil t))

;; Prevent issues with the Windows null device (NUL)
;; when using cygwin find with rgrep.
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
        ad-do-it))
(ad-activate 'grep-compute-defaults)

(add-to-list 'auto-mode-alist '("\\.bat\\'" . bat-mode))

;; set the active region color
;; http://stackoverflow.com/questions/18684579/how-do-i-change-the-highlight-color-for-selected-text-with-emacs-deftheme
;; check colors using 'list-colors-display'
(set-face-attribute 'region nil :background "#cd5b45")
