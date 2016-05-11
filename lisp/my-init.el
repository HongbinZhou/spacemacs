
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
  (global-set-key (kbd "M-i") 'helm-occur)
  (global-set-key (kbd "C-c M-i") 'helm-imenu))

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
(add-hook 'python-mode-hook
          '(lambda () (define-key python-mode-map
                        (kbd "C-c C-b") 'python-toggle-breakpoint)))

;; rails breakpoint key,
;; C-c C-b was shadowed by ruby-send-block??
(add-hook 'ruby-mode-hook
          '(lambda () (define-key ruby-mode-map
                        (kbd "C-c C-c") 'ruby-add-breakpoint)))

(defun ruby-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "byebug")
  (highlight-lines-matching-regexp "^[ ]*byebug"))

;; neat function to calc char occurences
;; http://emacs.stackexchange.com/questions/8067/how-to-obtain-the-statistics-of-the-frequency-of-characters-in-a-buffer/8071#8071
(defun char-stats (&optional case-sensitive)
  (interactive "P")
  (message "case-sensitive: %s" case-sensitive)
  (let ((chars (make-char-table 'counting 0)) 
        current)
    (cl-labels ((%collect-statistics
                 ()
                 (goto-char (point-min))
                 (while (not (eobp))
                   (goto-char (1+ (point)))
                   (setf current (preceding-char))
                   (set-char-table-range
                    chars current
                    (1+ (char-table-range chars current))))))
      (if case-sensitive
          (save-excursion (%collect-statistics))
        (let ((contents (buffer-substring-no-properties
                         (point-min) (point-max))))
          (with-temp-buffer
            (insert contents)
            (upcase-region (point-min) (point-max))
            (%collect-statistics)))))
    (with-current-buffer (get-buffer-create "*character-statistics*")
      (erase-buffer)
      (insert "| character | occurences |
               |-----------+------------|\n")
      (map-char-table
       (lambda (key value)
         (when (and (numberp key) (not (zerop value)))
           (cl-case key
             (?\n)
             (?\| (insert (format "| \\vert | %d |\n" value)))
             (otherwise (insert (format "| '%c' | %d |\n" key value))))))
       chars)
      (org-mode)
      (indent-region (point-min) (point-max))
      (goto-char 100)
      (org-cycle)
      (goto-char 79)
      (org-table-sort-lines nil ?N))
    (pop-to-buffer "*character-statistics*")))

;; thanks: https://gist.github.com/zdavkeos/1279865
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-region* - Diff two regions
;;
;;  To compare two regions, select the first region
;; and run `diff-region`.  The region is now copied
;; to a seperate diff-ing buffer.  Next, navigate
;; to the next region in question (even in another file).
;; Mark the region and run `diff-region-now`, the diff
;; of the two regions will be displayed by ediff.
;;
;;  You can re-select the first region at any time
;; by re-calling `diff-region`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p)  ; there is a region
        (let (buf)
          (setq buf (get-buffer-create "*Diff-regionA*"))
          (save-current-buffer
            (set-buffer buf)
            (erase-buffer))
          (append-to-buffer buf (region-beginning) (region-end)))
        )
  (message "Now select other region to compare and run `diff-region-now`")
  )

(defun diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
        (let (bufa bufb)
          (setq bufa (get-buffer-create "*Diff-regionA*"))
          (setq bufb (get-buffer-create "*Diff-regionB*"))
          (save-current-buffer
            (set-buffer bufb)
            (erase-buffer))
          (append-to-buffer bufb (region-beginning) (region-end))
          (ediff-buffers bufa bufb))
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; eval perl
;;; ref: http://stackoverflow.com/questions/12408031/emacs-perl-mode-send-script-buffer-to-perl-interpreter
(defun perl-on-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "perl" "*Perl Output*")
  (display-buffer "*Perl Output*"))

(eval-after-load 'perl-mode
  '(define-key perl-mode-map (kbd "C-c C-c") 'perl-on-buffer))


;;; toggle-window-split
;;; ref: https://www.reddit.com/r/emacs/comments/4dgxd6/weekly_emacs_questions_thread/
(defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
            (if (= (car this-win-edges)
                   (car (window-edges (next-window))))
                'split-window-horizontally
              'split-window-vertically)))
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1))))))

;; cperl
(defun my-cperl-mode-hook ()
  (setq cperl-indent-level 2
        cperl-close-paren-offset -2
        cperl-continued-statement-offset 2
        cperl-indent-parens-as-block t
        cperl-tab-always-indent t)
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

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

;; Git rid of weird behavior in interactive-haskell repl
;; It should caused by the non-ascii "lambda" in the prompts,
;; so just replace use just "> "
(setq haskell-interactive-prompt "> ")
