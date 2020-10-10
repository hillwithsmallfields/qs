(+ (* 3 4) 5)

(* (+ 3 4) 5)

(let ((a (+ 3 4))
      (b (* 3 4)))
  (/ b a))

(builtins)

(let ((financisto (read-canonical "~/common/qs/qs-scratch/financisto-full.csv"))
      (handelsbanken (read-canonical "~/common/qs/qs-scratch/handelsbanken-full.csv")))
  (print "writing financisto-as-csv.csv with headers" (headers financisto))
  (write-csv financisto "/tmp/fintest/financisto-as-csv.csv")

  (print "charting handelsbanken")
  (chart handelsbanken "Statement data" "/tmp/fintest/handelsbanken.gnuplot" '("balance"))

  (print "filtering app data")
  (write-csv (filter-sheet financisto "account" "Sterling cash")
             "/tmp/fintest/app-sterling.csv")

  (print "writing handelsbanken-as-csv.csv with headers" (headers handelsbanken))
  (write-csv handelsbanken "/tmp/fintest/handelsbanken-as-csv.csv")

  (print "converting bank statement from sheet to account")
  (let ((statement-as-account (account "HB current statement" handelsbanken)))
    (print "writing handelsbanken-as-account.csv with" (length (payees statement-as-account)) "payees")
    (write-csv statement-as-account "/tmp/fintest/handelsbanken-as-account.csv"))

  (print "converting app data from sheet to account")
  (let ((app-as-account (account "Handelsbanken current account" financisto)))

    (print "writing financisto-as-account.csv with" (length (payees app-as-account)) "payees")
    (write-csv app-as-account "/tmp/fintest/financisto-as-account.csv")

    (print "making monthly summary")
    (let ((monthly (by-month app-as-account)))
      (write-csv monthly "/tmp/fintest/financisto-monthly.csv")
      (let ((monthly-sheet (account-to-sheet monthly)))
        (write-csv monthly-sheet "/tmp/fintest/financisto-monthly-sheet.csv")))

    (print "merging sheet" handelsbanken "into account" app-as-account)
    (let ((added-to-account (add-sheet app-as-account handelsbanken '("regular") "/tmp/fintest/trace.csv")))

      (print "added to account" added-to-account)
      (write-csv added-to-account "/tmp/fintest/added-to-account.csv")))

  (let ((tracked-hb (track handelsbanken "amount" "tracked-amount")))
    (write-all-columns tracked-hb "/tmp/fintest/tracked-hb.csv"))

  (print "Categorising from app")
  (write-csv (categories financisto) "/tmp/fintest/financisto-category-tree.csv"))
