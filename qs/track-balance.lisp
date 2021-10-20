(let* ((main (read-canonical main-file))
       (filtered (fgrep main "Handelsbanken current account" "account"))
       (tracked (track filtered "amount" "balance"))
       (raw-statement (read statement-file))
       (statement (rename-column (select-columns raw-statement
                                                 '("Balance"))
                                 "Balance" "statement"))
       (transactions-month-ends (last-of-month (select-columns tracked '("balance"))))
       (statement-month-ends (last-of-month statement))
       (merged-month-ends (join-by-months transactions-month-ends statement-month-ends))
       )
  (print "saving tracking results")
  (write-csv main "main.csv")
  (write-csv filtered "filtered.csv")
  (write-csv tracked "tracked.csv")
  (write-csv raw-statement "raw-statement.csv")
  (write-csv statement "statement.csv")
  (write-csv statement-month-ends "statement-month-ends.csv")
  (write-csv transactions-month-ends "transation-month-ends.csv")
  (write-csv merged-month-ends "merged-month-ends.csv")
  (let* ((continuous-comparison (compare "discrepancy"
                                         filtered "amount" "tracked balance" nil nil
                                         statement nil "statement" nil nil))
         (annotated-cont-comp (annotate-by-timestamp
                               (annotate-by-timestamp continuous-comparison
                                                     filtered
                                                     '("payee" "category" "note"))
                               raw-statement '("Details"))))
    (write-csv continuous-comparison "continuous-comparison.csv")
    (write-csv annotated-cont-comp "annotated-cont-comp.csv")
    ))
