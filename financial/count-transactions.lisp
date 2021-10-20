(write-csv
 (count-month-transactions
  (read-canonical
   "$COMMON/finances/handelsbanken/handelsbanken-full.csv"))
 "monthly-counts.csv")
