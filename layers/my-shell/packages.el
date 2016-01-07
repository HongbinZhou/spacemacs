;;; packages.el --- my-shell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-shell-packages
    '(
      eshell
      ))

;; List of packages to exclude.
(setq my-shell-excluded-packages '())

(defun my-shell/init-eshell()
  (use-package eshell
    :defer t
    :init
    (progn
      (setq eshell-directory-changed t)

      (defun spacemacs//eshell-auto-rename-buffer-output-filter (text)
        (if (and (eq major-mode 'shell-mode)
                 eshell-directory-changed)
            (progn
              (let ((bn  (concat "eshell-hbzhou:" default-directory)))
                (if (not (string= (buffer-name) bn))
                    (rename-buffer bn t)))
              (setq eshell-directory-changed nil))))


      (defun spacemacs//eshell-auto-rename-buffer-input-filter (text)
        (if (eq major-mode 'shell-mode)
            (if ( string-match "^[ \t]*cd *" text)
                (setq eshell-directory-changed t))))

      (add-hook 'comint-output-filter-functions 'spacemacs//eshell-auto-rename-buffer-output-filter)
      (add-hook 'comint-input-filter-functions 'spacemacs//eshell-auto-rename-buffer-input-filter)

       ;;; ref: https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
      (defun spacemacs//eshell-here ()
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

      (global-set-key (kbd "C-c C-z") 'spacemacs//eshell-here)
      (global-set-key (kbd "C-c z") 'shell)

      (add-hook 'eshell-mode-hook
                #'(lambda ()
                    (define-key eshell-mode-map (kbd "M-r")  'helm-eshell-history)))
    )
    )
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
