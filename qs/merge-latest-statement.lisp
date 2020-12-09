(let* ((main (read-canonical "~/common/finances/finances.csv"))
       (summarised (by-day main))
       (latest (read-canonical "~/Downloads/Transactions.csv")))
  (print "Number of original entries:" (length main) " Number of summarised entries:" (length summarised))
  (for-each-row latest this-row nil
                (let* ((amount (get this-row "amount"))
                       (matches (find-amount summarised amount (get this-row "timestamp") 7)))
                  (if (> (length matches) 0)
                      (progn
                        (print "got matches on amount" amount "for payee" (get this-row "payee") "on" (get this-row "date"))
                        (dolist (match matches)
                          (print "    payee" (get match "payee") "on" (get match "date"))))
                    (print "no matches on amount" amount "for payee" (get this-row "payee") "on" (get this-row "date")))
                  )))
