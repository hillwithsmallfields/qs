(let* ((period (getenv "PERIOD" "full"))
       (output-dir (format "/tmp/finrun-%s" period))
       (financisto-basic (read-canonical
                          (format "~/common/qs/qs-scratch/financisto-%s.csv"
                                  period)))
       (financisto-hb-as-account (account "Handelsbanken current account" financisto-basic))
       (financisto-monthly (by-month financisto-basic))
       (financisto-hb-monthly (account-to-sheet (by-month financisto-hb-as-account)))
       (category-tree (categories financisto))
       (category-sheet (categorised financisto)))
  (write-csv category-sheet "/tmp/finrun/category-sheet.csv")
  (write-csv category-tree "/tmp/finrun/category-tree.csv"))
