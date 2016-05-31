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

(defun hbzhou/indent-all ()
  "Indent entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun hbzhou/untabify-all ()
  "untabify entire buffer"
  (interactive)
  (save-excursion
    (if (not indent-tabs-mode)
        (untabify (point-min) (point-max)))
    nil))

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

