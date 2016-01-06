;;; delete trailing space of given file
(defun hbzhou/delete-trailing-space-file (file)
  (interactive "F")
  (save-excursion
    (find-file file)
    (delete-trailing-whitespace)
    (write-file file)
    (kill-buffer (current-buffer))
    ))

;;; delete trailing space for all el files in given dir
(defun hbzhou/delete-trailing-space-dir-el (dir)
  (interactive "D")
  (mapc 'hb/delete-trailing-space-file
        (directory-files dir t ".el$")))

