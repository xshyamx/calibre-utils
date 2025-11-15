;;; book.el --- Fix book list              -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Fix the book names from the calibre metadata

;;; Code:
(require 'cl-macs)
(require 'ucs-normalize)

(defconst book-directory-book-count 2
  "Number of standalone books by an author above which an author directory
will be created")

(defun book-kebab-case (s)
  "Convert to kebab case by splitting on spaces, underscores &
  hyphens"
  (save-match-data
    (downcase
     (mapconcat
      (lambda (s) (replace-regexp-in-string "[^[:alnum:]]+" "" s))
      (split-string (string-replace "-" " " (book-normalize-string s)) "[[:space:]]+")
      "-"))))

(defun book-series (i)
  "Convert the series string to a 2-digit 0 prefixed index"
  (let* ((x (truncate i))
	 (y (truncate (- (* 10 i) (* 10 x)))))
    (concat (format "%02d" x)
	    (if (> y 0) (format "-%02d" y) ""))))

(defun book-normalize-string (s)
  "Remove non-ascii unicode characters"
  (replace-regexp-in-string
   "[^[:ascii:]]" ""
   (ucs-normalize-NFD-string s)))

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
    (insert "if [ ! -d ./raw ]; then\n")
    (insert "  echo \"Source directory does not exist. Run 'make raw'.\"\n")
    (insert "  exit 1\n")
    (insert "fi\n\n")
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
    (dolist (author-books (seq-group-by (lambda (b) (nth 2 b)) (sqlite-select db standalone-sql)))
      (let ((dir (if (> (length (cdr author-books)) book-directory-book-count)
		     (book-kebab-case (car author-books)) nil))
	    (target-dir))
	(unless (or (null dir) (member dir dirs))
	  (push dir dirs)
	  (insert (format "mkdir -p %s/%s\n" out-dir dir)))
	(setq target-dir (if dir (format "%s/%s" out-dir dir) out-dir))
	(dolist (book (cdr author-books))
	  (cl-destructuring-bind (id title author) book
	    (let ((file (book-file title author)))
	      (unless (member file files)
		(push file files)
		(insert (format "cp \"raw/%s.epub\" \"%s/%s\"\n"
				(alist-get id book-files)
				target-dir file))))))))
    (write-file "generate.sh")))

(provide 'book)
;;; book.el
