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
  (write-csv (fgrep financisto "Sterling cash" "account")
             "/tmp/fintest/app-sterling.csv")
  (print "filtering statement data")
  (let ((paypal-all (grep handelsbanken "PAYPAL PAYMENT")))
    (write-csv paypal-all
               "/tmp/fintest/paypal.csv")
    (print "selecting columns of filtered statement data")
    (let ((paypal-selected (select-columns paypal-all
                                           '("amount"))))
      (write-csv paypal-selected
                 "/tmp/fintest/paypal-selected.csv")))

  (print "writing handelsbanken-as-csv.csv with headers" (headers handelsbanken))
  (write-csv handelsbanken "/tmp/fintest/handelsbanken-as-csv.csv")

  (print "converting bank statement from sheet to account")
  (let ((statement-as-account (account "HB current statement" handelsbanken)))
    (print "writing handelsbanken-as-account.csv with" (length (payees statement-as-account)) "payees")
    (write-csv statement-as-account "/tmp/fintest/handelsbanken-as-account.csv")

    (print "converting app data from sheet to account")
    (let ((app-as-account (account "Handelsbanken current account" financisto)))

      (print "writing financisto-as-account.csv with" (length (payees app-as-account)) "payees")
      (write-csv app-as-account "/tmp/fintest/financisto-as-account.csv")

      (print "making monthly summaries")
      (let ((app-monthly (by-month app-as-account))
            (statement-monthly (by-month statement-as-account)))
        (write-csv app-monthly "/tmp/fintest/financisto-monthly.csv")
        (write-csv statement-monthly "/tmp/fintest/handelsbanken-monthly.csv")
        (let ((app-monthly-sheet (account-to-sheet app-monthly))
              (statement-monthly-sheet (account-to-sheet statement-monthly)))
          (write-csv app-monthly-sheet "/tmp/fintest/financisto-monthly-sheet.csv")
          (write-csv statement-monthly-sheet "/tmp/fintest/handelsbanken-monthly-sheet.csv")
          (write-json app-monthly-sheet "/tmp/fintest/financisto-monthly-sheet.json")
          (write-json statement-monthly-sheet "/tmp/fintest/handelbanken-monthly-sheet.json")
          (let ((monthly-difference (subtract-cells app-monthly-sheet statement-monthly-sheet)))
            (write-csv monthly-difference "/tmp/fintest/monthly-difference.csv")
            (let ((non-trivial-monthly-differences (threshold monthly-difference 10)))
              (write-csv non-trivial-monthly-differences "/tmp/fintest/monthly-difference-over-10.csv")
              (let ((occupied-non-trivial-monthly-differences (occupied-columns non-trivial-monthly-differences)))
                (write-csv occupied-non-trivial-monthly-differences "/tmp/fintest/occupied-monthly-difference-over-10.csv"))))))

      (print "merging sheet" handelsbanken "into account" app-as-account)
      (let ((added-to-account (add-sheet app-as-account handelsbanken '("regular") "/tmp/fintest/trace.csv")))

        (print "added to account" added-to-account)
        (write-csv added-to-account "/tmp/fintest/added-to-account.csv"))))

  (let ((tracked-hb (track handelsbanken "amount" "tracked-amount")))
    (write-all-columns tracked-hb "/tmp/fintest/tracked-hb.csv"))

  (print "Categorising from app")
  (write-csv (categories financisto) "/tmp/fintest/financisto-category-tree.csv")

  (print "Subtracting statement from app")
  (let ((difference (subtract-cells financisto handelsbanken)))
    (write-csv difference "/tmp/fintest/raw-difference")
    (print "Thresholding difference")
    (let ((most (threshold difference 10)))
      (write-csv most "/tmp/fintest/difference-over-10"))))
