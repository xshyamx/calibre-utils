;;; book.el --- Fix book list              -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Fix the book names from the calibre metadata

;;; Code:
(require 'cl-macs)

(defun book-kebab-case (s)
  "Convert to kebab case by splitting on spaces, underscores &
  hyphens"
  (save-match-data
    (replace-regexp-in-string (rx bow (group alpha) "-" (group alpha) "-") "\\1\\2-"
			      (string-join (split-string (string-replace "'" "" (downcase s)) "[^[:word:]]+") "-"))))

(defun book-series (i)
  "Convert the series string to a 2-digit 0 prefixed index"
  (let* ((x (truncate i))
	 (y (truncate (- (* 10 i) (* 10 x)))))
    (concat (format "%02d" x)
	    (if (> y 0) (format "-%02d" y) ""))))

(defun book-file (title author &optional series index)
  (if (null series)
      (format "%s--%s.epub" (book-kebab-case author) (book-kebab-case title))
    (format "%s--%s--%s.epub" (book-series index) (book-kebab-case author) (book-kebab-case title))))


(let ((db (sqlite-open "metadata.db"))
      (file-sql "select book, name from data where format=?")
      (series-sql "select b.id, b.title, a.name, b.series_index, s.name from
  books as b
  inner join books_authors_link as bal on bal.book = b.id
  inner join authors as a on a.id = bal.author
  inner join books_series_link as bsl on bsl.book = b.id
  inner join series as s on s.id = bsl.series
  where bal.id = ( select min(id) from books_authors_link where book = b.id )")
      (standalone-sql "select b.id, b.title, a.name  from
  books as b
  inner join books_authors_link as bal on bal.book = b.id
  inner join authors as a on a.id = bal.author
  where bal.id = ( select min(id) from books_authors_link where book = b.id )
        and not exists (select 1 from books_series_link where book = b.id)")
      (out-dir "out")
      (file) (dirs) (book-files) (files))
  (setq book-files (mapcar (lambda (l) (cons (nth 0 l) (nth 1 l)))
			   (sqlite-select db file-sql ["EPUB"])))
  (with-temp-buffer
    (insert "#!/bin/sh -e\n\n")
    (insert (format "mkdir -p %s\n" out-dir))
    (insert "\n# Series books\n\n")
    (dolist (row (sqlite-select db series-sql))
      (cl-destructuring-bind (id title author index series) row
	(let ((dir (book-kebab-case series))
	      (file (book-file title author series index)))
	  (unless (member dir dirs)
	    (push dir dirs)
	    (insert (format "mkdir -p %s/%s\n" out-dir dir)))
	  (unless (member file files)
	    (push file files)
	    (if (alist-get id book-files)
		(insert (format "cp \"raw/%s.epub\" \"%s/%s/%s\"\n"
				(alist-get id book-files)
				out-dir dir file)))))))

    (insert "\n# Standalone books\n\n")
    (dolist (row (sqlite-select db standalone-sql))
      (cl-destructuring-bind (id title author) row
	(let ((file (book-file title author)))
	  (unless (member file files)
	    (push file files)
	    (insert (format "cp \"raw/%s.epub\" \"%s/%s\"\n"
			    (alist-get id book-files)
			    out-dir file))))))
    (write-file "generate.sh")))

(provide 'book)
;;; book.el
