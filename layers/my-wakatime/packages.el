;;; packages.el --- my-wakatime layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <hongbinzhou@hotmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-wakatime-packages '(wakatime-mode f s))

(defun my-wakatime/init-wakatime-mode ()
  (use-package wakatime-mode
    :defer t
    :init
    (setq wakatime-cfg "~/.wakatime.cfg")
    (setq wakatime-bin-name 
          (if (spacemacs/system-is-mswindows)
              "wakatime-script.py"
            "wakatime"))
    (setq wakatime-bin (locate-file wakatime-bin-name exec-path))
    (when (and (file-exists-p wakatime-bin)
               (file-exists-p wakatime-cfg))
      (progn
        (setq-default wakatime-cli-path wakatime-bin)
        (setq-default wakatime-api-key
                      (cadr (s-match "api_key\\s-*=\\s-*\\(.*\\)\\s-*"
                                     (with-temp-buffer
                                       (insert-file-contents wakatime-cfg)
                                       (buffer-string)))))
        (add-hook 'prog-mode-hook 'wakatime-mode)))
    :config
    (defun spacemacs/wakatime-dashboard ()
      (interactive)
      (browse-url "http://wakatime.com/dashboard"))
    (spacemacs/set-leader-keys
      "aW" 'spacemacs/wakatime-dashboard)
    (spacemacs|diminish wakatime-mode " Ⓦa" " Wa")))
