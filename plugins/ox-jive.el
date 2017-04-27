;;; ox-jive.el --- a package to convert org exported html for jive html

;; Copyright (C) 2016  Hongbin Zhou

;; Author: Hongbin Zhou <hongbinzhou@hotmail.com>
;; Keywords: ox-jive
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org file can be easily exported to html, however, jive doesn't support all
;; the org exported html tags. This package did modifications on the org
;; exported html, then output html will be well displayed in jive html.

;;; Code:

;; code goes here

;;;--------------------------------------------------
;;; functions for publish html on the voice
;;;--------------------------------------------------

;;; delete given string in current buffer
(defun org-jive-string-replace (src tgt)
  (goto-char 1)
  (while (search-forward-regexp src nil t)
    (replace-match tgt t nil)))

;; replace <br /> by <br/> in org-mode exported html buffer
(defun org-jive-html-fix-<br> ()
  (org-jive-string-replace "\\(<br[[:space:]]*/>\\)" "<br/>"))

;; add class="j-table jiveBorder" to <table> element
(defun org-jive-html-add-class-to-<table> ()
  (org-jive-string-replace "\\(<table\\)" "<table class=\"j-table jiveBorder\""))

;; delete "<colgroup> ... </colgroup>" in org-mode exported html buffer
(defun org-jive-html-delete-<colgroup> ()
  (org-jive-string-replace "\\(<colgroup>[\0-\377[:nonascii:]]*?</colgroup>\\)" ""))

;; delete "<head> ... </head>" in org-mode exported html buffer
(defun org-jive-html-delete-<head> ()
  (org-jive-string-replace "\\(<head>[\0-\377[:nonascii:]]*?</head>\\)" ""))

;; delete "<table-of-contents> ... </table-of-contents>" in org-mode exported html buffer
(defun org-jive-html-delete-<table-of-contents> ()
  (org-jive-string-replace "\\(<div id=\"text-table-of-contents\">[\0-\377[:nonascii:]]*?</div>\\)" "")
  (org-jive-string-replace "\\(<div id=\"table-of-contents\">[\0-\377[:nonascii:]]*?</div>\\)" ""))

;; delete "<meta />" in org-mode exported html buffer
(defun org-jive-html-delete-<meta> ()
  (org-jive-string-replace "\\(<meta.*/>\\)" ""))

;; delete "<div id="postamble" "status"> ... </div>" in org-mode exported html buffer
(defun org-jive-html-delete-<div-postamble> ()
  (org-jive-string-replace "\\(<div id=\"postamble\" class=\"status\">[\0-\377[:nonascii:]]*?</div>\\)" ""))

;; add <p/> before each header to make page nicer
(defun org-jive-html-add-<p>-before-header ()
  (org-jive-string-replace "\\(<div id=\"outline-container\\)" "<p/>\n\\1"))

;; convert org-src to jive_macro_code
(defun org-jive-html-org-src-to-jive-code ()
  (org-jive-string-replace "<pre class=\"src src-\\(.*\\)\">"
                           "<pre class=\"language-\\1 line-numbers\">"))

(defun org-jive-add-<br/>-to-jive-src ()
  (goto-char 1)
  (let ((jivetag "\\(<pre class=\"jive_text_macro jive_macro_code\" jivemacro=\"code\" ___default_attr=\".*\">\\)\\([\0-\377[:nonascii:]]*?\\)\\(</pre>\\)"))
    (save-match-data
      (while (search-forward-regexp jivetag nil t)
        (let ((begin (match-string 1))
              (items (match-string 2))
              (end (match-string 3)))
          (let ((items-new (replace-regexp-in-string "$" "<br/>" items)))
            (let ((output (concat begin items-new end)))
              (replace-match "" t nil)
              (insert output)
              ;; (message "output: %s" output))
            )))))))

;; add the voice toc
(defun org-jive-html-add-toc ()
  (org-jive-string-replace "\\(<div id=\"content\">\\)"
                         "<div id=\"content\">\n<p><img class=\"jive_macro jive_macro_toc\" jivemacro=\"toc\" /></p>"))

;; make the voice html happy
(defun org-jive-html-make-thevoice-happy ()
  (interactive)
  (org-jive-html-delete-<head>)
  (org-jive-html-delete-<meta>)
  (org-jive-html-delete-<table-of-contents>)
  (org-jive-html-fix-<br>)
  (org-jive-html-delete-<colgroup>)
  (org-jive-html-delete-<div-postamble>)
  (org-jive-html-add-<p>-before-header)
  (org-jive-html-org-src-to-jive-code)
  (org-jive-html-add-class-to-<table>)
  (org-jive-html-add-toc))

(provide 'ox-jive)
;;; end
