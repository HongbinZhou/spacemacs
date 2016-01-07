
;;; http://ergoemacs.org/emacs/emacs_unix.html
;; by Ellen Taylor, 2012-07-20
(defadvice shell (around always-new-shell)
  "Always start a new shell."
  (let ((buffer (generate-new-buffer-name "*shell*"))) ad-do-it))

(ad-activate 'shell)
