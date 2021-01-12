(let* ((raw (read-canonical input-file))
       (monthly (check "making monthly"
                       (by-month (fgrep raw "GBP" "currency")
                                 nil nil)))
       (parentage-table (read-parentage-table "cats.yaml"))
       (classifiers (read-classifier classifiers-file))
       (unclassified (unclassified-categories classifiers parentage-table))
       (automatic-categories (flagged-categories "handelsbanken" "automatic"))
       (by-category (categorised monthly))
       ;; (by-parentage (by-parent monthly parentage-table))
       (by-class (check "classified" (by-classification monthly parentage-table classifiers t nil)))
       (automatics (by-classification monthly parentage-table automatic-categories t nil))
       (by-level-0 (by-hierarchy monthly 0 parentage-table))
       (by-level-1 (by-hierarchy monthly 1 parentage-table))
       (by-level-2 (by-hierarchy monthly 2 parentage-table))
       (by-proportions (proportions by-category)))
  (print "parentage table is" parentage-table)
  (print "classifiers are:" classifiers)
  (print "unclassified categories are" unclassified)
  (print "automatic categories are" automatic-categories)
  (write-html raw "raw.html" "Raw data" t t)
  (write-csv monthly "monthly.csv")
  (write-html monthly "monthly.html" "Raw monthly summary" t t)
  (write-csv by-category "by-category.csv")
  ;; (write-csv by-parentage "by-parentage.csv")
  (write-csv-with-averages by-class "by-class.csv")
  (write-html by-class "by-class.html" "Categorised monthly summary" t t)
  (write-csv automatics "automatics.csv")
  (write-csv by-level-0 "by-level-0.csv")
  (write-csv by-level-1 "by-level-1.csv")
  (write-csv by-level-2 "by-level-2.csv")
  (write-csv by-proportions "by-proportions.csv"))
