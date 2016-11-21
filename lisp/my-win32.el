(setenv "PATH"
	(concat
	 "C:/Python27" ";"
	 "C:/cygwin64/bin" ";"
	 "C:/cygwin64/usr/sbin" ";"
   "C:/Program Files (x86)/Graphviz2.38/bin" ";" ;for dot graphviz
   "C:\\Program Files (x86)\\Pandoc" ";"         ;pandoc
	 (getenv "PATH")
	 ))

(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict")
;; use American English as ispell default dictionary
(ispell-change-dictionary "american" t)
(setq magit-git-executable "C:/Program Files/Git/bin/git.exe")

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

(use-package bat-mode
  :mode "\\.bat\\'")

;; set the active region color
;; http://stackoverflow.com/questions/18684579/how-do-i-change-the-highlight-color-for-selected-text-with-emacs-deftheme
;; check colors using 'list-colors-display'
(set-face-attribute 'region nil :background "#cd5b45")

;; make org-babel works in Windows
(setq default-buffer-file-coding-system 'utf-8-unix)

;; support launch cygwin bash
(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash")
        (explicit-bash-args '("--login" "-i")))
    (call-interactively 'shell)))
(global-set-key (kbd "C-c C-z") 'cygwin-shell)
