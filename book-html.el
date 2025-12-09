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
	     " & "))

(unless (< 0 (length argv))
  (message "metadata.db file required")
  (kill-emacs 1))

;; Sometimes opening metadat.db stalls with warning about loading
;; large files. Uncomment the warning setting to proceed in such
;; scenarios
;(setq large-file-warning-threshold nil)

(let ((db (sqlite-open (car argv)))
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
    .author, .series { width: 20% }
    .title { width: 40% }
    </style>
  </head>
  <body>
    <table>
      <thead>
	<tr>
          <th>Book ID</th>
          <th class=\"author\">Author</th>
          <th class=\"title\">Title</th>
          <th class=\"series\">Series</th>
          <th>Series Number</th>
	</tr>
      </thead>
      <tbody>
      </tbody>
    </table>
    <script>
let books = [\n")
    (insert (mapconcat
	     (lambda (row)
	       (cl-destructuring-bind (id author title series index) row
		 (format "  [\"%s\", %S, %S, %S, \"%s\"]" id (book-html-author author) title (if (null series) "" series) (if (null series) "" index))))
	     (sqlite-select db sql)
	     ",\n"))
    (insert "\n]
function addBooks() {
    var tb = document.querySelector('tbody')
    var tt = document.createElement('tr')
    tt.innerHTML = '<td></td><td></td><td></td><td></td><td></td>'
    books.forEach(book => {
	var tr = tt.cloneNode(true)
	var tds = tr.querySelectorAll('td')
	book.forEach((e,i)=>{
	    tds[i].textContent = e
	})
	tb.appendChild(tr)
    })
}
window.addEventListener('load', addBooks)
   </script>
  </body>
</html>")
    (write-file html-file)))



(provide 'book-html)
;;; book-html.el
