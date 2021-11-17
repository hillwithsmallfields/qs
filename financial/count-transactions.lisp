(write-csv
 (count-month-transactions
  (read-canonical
   "$SYNCED/finances/handelsbanken/handelsbanken-full.csv"))
 "monthly-counts.csv")
