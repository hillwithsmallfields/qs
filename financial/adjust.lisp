(let* ((main (read-canonical "$COMMON/finances/finances.csv"))
       (statement (read-canonical "$COMMON/finances/handelsbanken/handelsbanken-full.csv"))
       (adjustments-main (adjustments-by-month main statement "Handelsbanken current account"))
       (merged (replace-matching-rows main adjustments-main '("date" "category"))))
  (write-csv adjustments-main "/tmp/adjustments.csv")
  (write-csv merged "/tmp/merged.csv"))
