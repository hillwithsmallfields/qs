(let* ((main (read-canonical "~/common/finances/finances.csv"))
       (summarised (by-day main t))
       (latest (read-canonical incoming-statement))
       (unmatched (blank-sheet))
       (regular-entries (blank-sheet))
       (unambiguously-matched (blank-sheet))
       (ambiguously-matched (blank-sheet)))
  (print "Number of original entries:" (length main) " Number of summarised entries:" (length summarised))
  (for-each-row latest this-row nil
                (let* ((amount (get this-row "amount"))
                       (matches (find-amount summarised amount (get this-row "timestamp") 7)))
                  (if (> (length matches) 0)
                      (let ((found (find-by-field (get this-row "payee") matches "payee")))
                        (when verbose
                          (print "got matches on amount" amount
                                 "for payee" (get this-row "payee")
                                 "on" (get this-row "date")
                                 "by field" found)
                          (dolist (match matches)
                            (print "    payee" (get match "payee")
                                   "on" (get match "date"))))
                        (if (> (length matches) 1)
                            (add-row ambiguously-matched this-row)
                          (add-row unambiguously-matched this-row)))
                    (let* ((payee (get this-row "payee"))
                           (regular (flagged-as "handelsbanken" payee "regular")))
                      (when verbose
                        (print "no matches on amount" amount
                               "for payee" payee
                               "regular" regular
                               "on" (get this-row "date")))
                      (if regular
                          (add-row regular-entries this-row)
                        (when verbose
                          (print "unmatched" unmatched "this-row" this-row))
                        (add-row unmatched this-row))))
                  ))
  (print (length unmatched) "non-regular unmatched transactions")
  (write-csv unmatched "unmatched-irregular.csv")
  (print (length regular-entries) "regular unmatched transactions")
  (write-csv regular-entries "unmatched-regular.csv")
  (print (length ambiguously-matched) "ambiguously matched transactions")
  (write-csv ambiguously-matched "ambiguously-matched.csv")
  (print (length unambiguously-matched) "unambiguously matched transactions")
  (write-csv unambiguously-matched "unambiguously-matched.csv"))
