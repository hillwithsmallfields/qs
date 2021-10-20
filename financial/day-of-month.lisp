;; Program to prepare CSV files for making charts, and some HTML files
;; for incorporating in my personal dashboard page.

(let* ((raw (read-canonical input-file))
       (sterling (fgrep raw "GBP" "currency"))
       (sterling-by-day (by-day-of-month sterling))
       )
  (write-csv sterling "sterling.csv")
  (sample sterling-by-day "sterling by day" 12)
  (write-csv sterling-by-day "by-day-of-month.csv")
  )
