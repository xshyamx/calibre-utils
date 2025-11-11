;;; book-html.el --- Generate html file from Calibre database  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Generate html file from Calibre database

;;; Code:
(require 'cl-macs)


(defun book-html--author-unsort (s)
  (let ((ps (split-string s ", ")))
    (string-join (append (cdr ps) (list (car ps))) " ")))

(defun book-html-author (s)
  (mapconcat #'book-html--author-unsort
	     (split-string s " & ")
	     " <wbr />& "))

(let ((db (sqlite-open "metadata.db"))
      (html-file "booklist.html")
      (sql "select b.id, b.author_sort as Author, b.title as Title, s.name as Series, b.series_index as Number from
	    books as b
	    inner join books_authors_link as bal on bal.book = b.id
	    inner join authors as a on a.id = bal.author
	    left join books_series_link as bsl on bsl.book = b.id
	    left join series as s on s.id = bsl.series
	    where bal.id = ( select min(id) from books_authors_link where book = b.id )
	    order by a.name, s.name, b.series_index"))

  (with-temp-buffer
    (insert "<!doctype html>
<html lang=\"en\">
  <head>
    <meta charset=\"UTF-8\"/>
    <title>Book List</title>
    <style>
    table { border-collapse: collapse; }
    th, td { border: 1px solid lightgrey; }
    th { background-color: whitesmoke; }
    td { padding: 0.2em 1em; }
    td:first-child { text-align: right; }
    tr:nth-child(even) { background-color: aliceblue; }
    </style>
  </head>
  <body>
    <table>
      <thead>
	<tr>
          <th>Book ID</th>
          <th>Author</th>
          <th>Title</th>
          <th>Series</th>
          <th>Series Number</th>
	</tr>
      </thead>
      <tbody>")
    (dolist (row (sqlite-select db sql))
      (cl-destructuring-bind (id author title series index) row
	(insert (format "	<tr>
	  <td>%s</td>
	  <td>%s</td>
	  <td>%s</td>
	  <td>%s</td>
	  <td>%s</td>
	</tr>
" id (book-html-author author) title (if (null series) "" series) (if (null series) "" index)))))
    (insert "      </tbody>
    </table>
  </body>
</html>")
    (write-file html-file)))



(provide 'book-html)
;;; book-html.el
