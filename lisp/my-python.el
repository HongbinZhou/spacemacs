
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-b" . python-add-breakpoint))
  :config
  (defun python-add-breakpoint ()
    "Add a break point"
    (interactive)
    (newline-and-indent)
    (insert "import pdb; pdb.set_trace()")
    (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))
  )

;; case 24401 begin: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=24401
(defun python-shell-completion-native-try ()
  "Return non-nil if can trigger native completion."
  (let ((python-shell-completion-native-enable t)
        (python-shell-completion-native-output-timeout
         python-shell-completion-native-try-output-timeout))
    (python-shell-completion-native-get-completions
     (get-buffer-process (current-buffer))
     nil "_")))
;; case 24401 done

;; (defun my-python-mode-hook ()
;;   (line-number-mode t)
;;   (hs-minor-mode t)
;;   (fold-dwim-org/minor-mode t)
;;   ;; handle trailing space
;;   (setq-default show-trailing-whitespace t)
;;   (setq-default require-final-newline t)
;;   (setq-default delete-trailing-lines nil)
;;   (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)
;;   )

;; (add-hook 'elpy-mode-hook
;;           'my-python-mode-hook)



;; (defun python-interactive ()
;;   "Enter the interactive Python environment"
;;   (interactive)
;;   (progn
;;     (insert "!import code; code.interact(local=vars())")
;;     (move-end-of-line 1)
;;     (comint-send-input)))

;; (global-set-key (kbd "C-c i") 'python-interactive)

;; ;; (require 'isend)
;; (setq isend-skip-empty-lines nil)
;; (setq isend-strip-empty-lines nil)
;; (setq isend-delete-indentation t)
;; (setq isend-end-with-empty-line t)

;; (defadvice isend-send (after advice-run-code-sent activate compile)
;;   "Execute whatever sent to the (Python) buffer"
;;   (interactive)
;;   (let ((old-buf (buffer-name)))
;;     (progn
;;       (switch-to-buffer isend--command-buffer)
;;       (goto-char (point-max))
;;       (comint-send-input)
;;       (switch-to-buffer old-buf))))

;; https://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5
;; IPython 5 has a new terminal interface, which is not compatible with Emacs'
;; inferior shells. To fix it, add the --simple-prompt flag to the IPython
;; command:
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
