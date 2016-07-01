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

;;;--------------------------------------------------
;;; functions for publish html on the voice
;;;--------------------------------------------------

;;; delete given string in current buffer
(defun hbzhou/string-replace (src tgt)
  (goto-char 1)
  (while (search-forward-regexp src nil t)
    (replace-match tgt t nil)))

;; replace <br /> by <br/> in org-mode exported html buffer
(defun hbzhou/html-fix-<br> ()
  (hbzhou/string-replace "\\(<br[[:space:]]*/>\\)" "<br/>"))

;; delete "<colgroup> ... </colgroup>" in org-mode exported html buffer
(defun hbzhou/html-delete-<colgroup> ()
  (hbzhou/string-replace "\\(<colgroup>[\0-\377[:nonascii:]]*?</colgroup>\\)" ""))

;; delete "<head> ... </head>" in org-mode exported html buffer
(defun hbzhou/html-delete-<head> ()
  (hbzhou/string-replace "\\(<head>[\0-\377[:nonascii:]]*?</head>\\)" ""))

;; delete "<meta />" in org-mode exported html buffer
(defun hbzhou/html-delete-<meta> ()
  (hbzhou/string-replace "\\(<meta.*/>\\)" ""))

;; delete "<div id="postamble" "status"> ... </div>" in org-mode exported html buffer
(defun hbzhou/html-delete-<div-postamble> ()
  (hbzhou/string-replace "\\(<div id=\"postamble\" class=\"status\">[\0-\377[:nonascii:]]*?</div>\\)" ""))

;; add <p/> before each header to make page nicer
(defun hbzhou/html-add-<p>-before-header ()
  (hbzhou/string-replace "\\(<div id=\"outline-container\\)" "<p/>\n\\1"))

;; add the voice toc
(defun hbzhou/html-add-toc ()
  (hbzhou/string-replace "\\(<div id=\"content\">\\)" 
                         "<div id=\"content\">\n<p><img class=\"jive_macro jive_macro_toc\" jivemacro=\"toc\" /></p><p/>"))

;; make the voice html happy
(defun hbzhou/html-make-thevoice-happy ()
  (interactive)
  (hbzhou/html-delete-<head>)
  (hbzhou/html-delete-<meta>)
  (hbzhou/html-fix-<br>)
  (hbzhou/html-delete-<colgroup>)
  (hbzhou/html-delete-<div-postamble>)
  (hbzhou/html-add-<p>-before-header)
  (hbzhou/html-add-toc))


;;; unfill paragraph
;;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
