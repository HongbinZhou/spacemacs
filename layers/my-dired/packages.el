;;; packages.el --- my-dired Layer packages File for Spacemacs
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
(setq my-dired-packages
    '(
      dired-async
      ))

;; List of packages to exclude.
(setq my-dired-excluded-packages '())

;; For each package, define a function my-dired/init-<package-name>
;;
(defun my-dired/init-my-package ()
  (use-package dired-async
    :defer t
    :init
    (autoload 'dired-async-mode "dired-async.el" nil t)
    (dired-async-mode 1)
    )
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
