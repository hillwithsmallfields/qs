(let* ((main (read-canonical "~/common/finances/finances.csv"))
       (summarised (by-day main))
       (latest (read-canonical "~/Downloads/Transactions.csv")))
  (print "Number of original entries:" (length main) " Number of summarised entries:" (length summarised))
  (for-each-row latest this-row nil
                (let* ((amount (get this-row "amount"))
                       (matches (find-amount summarised amount (get this-row "timestamp") 7)))
                  (if (> (length matches) 0)
                      (let ((found (find-by-field (get this-row "payee") matches "payee")))
                        (print "got matches on amount" amount
                               "for payee" (get this-row "payee")
                               "on" (get this-row "date")
                               "by field" found)
                        (dolist (match matches)
                          (print "    payee" (get match "payee")
                                 "on" (get match "date"))))
                    (let* ((payee (get this-row "payee"))
                           (regular (flagged-as "handelsbanken" payee "regular")))
                      (print "no matches on amount" amount
                             "for payee" payee
                             "regular" regular
                             "on" (get this-row "date"))))
                  )))
