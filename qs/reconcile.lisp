(let* ((handelsbanken-as-account (account "HB current statement"
                                          (read-canonical "~/common/qs/qs-scratch/handelsbanken-full.csv")))
       (financisto-as-account (account "Handelsbanken current account"
                                       (read-canonical "~/common/qs/qs-scratch/financisto-full.csv")))
       (financisto-monthly (account-to-sheet (by-month financisto-as-account)))
       (handelsbanken-monthly (account-to-sheet (by-month handelsbanken-as-account)))
       (monthly-difference (subtract-cells financisto-monthly handelsbanken-monthly))
       (non-trivial-monthly-differences (threshold monthly-difference 10))
       (occupied-non-trivial-monthly-differences (occupied-columns non-trivial-monthly-differences)))
  (write-csv monthly-difference "/tmp/finrun/monthly-difference.csv")
  (write-csv non-trivial-monthly-differences "/tmp/finrun/non-trivial-monthly-differences.csv")
  (write-json non-trivial-monthly-differences "/tmp/finrun/non-trivial-monthly-differences.json")
  (write-csv occupied-non-trivial-monthly-differences "/tmp/finrun/occupied-non-trivial-monthly-differences.csv")
  )
