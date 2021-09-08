;; Program to prepare CSV files for making charts, and some HTML files
;; for incorporating in my personal dashboard page.

(let* ((raw (read-canonical input-file))
       (sterling (fgrep raw "GBP" "currency"))
       (sterling-by-day (by-day-of-month sterling))
       (monthly (by-month sterling
                          nil nil))
       (parentage-table (read-parentage-table "cats.yaml"))
       (classifiers (read-classifier classifiers-file))
       (thresholds (read-thresholds thresholds-file))
       (unclassified (unclassified-categories classifiers parentage-table))
       (automatic-categories (flagged-categories "handelsbanken" "automatic"))
       (by-category (categorised monthly))
       (by-parentage (by-parent monthly parentage-table))

       (by-class (by-classification monthly parentage-table classifiers t nil)) ; TODO: this is putting everything from Amazon under "books"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! and also everything from 6MBCS as groceries, I think
       (by-class-this-month (this-month by-class))
       (by-class-past-three-months (past-months by-class 3))
       (by-class-past-year (past-months by-class 12))
       (by-class-this-year (this-year by-class))
       
       (automatics (by-classification monthly parentage-table automatic-categories t nil)) ; damages by-class?
       (by-level-0 (by-hierarchy monthly 0 parentage-table)) ; causes problems?
       (by-level-1 (by-hierarchy monthly 1 parentage-table))
       (by-level-2 (by-hierarchy monthly 2 parentage-table))
       (by-proportions (proportions by-category))

       (balance-from-statements (select-columns
                                 (read statements-file)
                                 '("Date" "Balance")))

       (monthly-balance-from-statements (last-of-month balance-from-statements))
       
       (with-last-of-month (join-by-months by-class monthly-balance-from-statements))
       )
  (when verbose
    (print "parentage table is" parentage-table)
    (print "classifiers are:" classifiers)
    (print "thresholds are:" thresholds)
    (print "unclassified categories are" unclassified)
    ;; (print "automatic categories are" automatic-categories)
    )
  ;; (sample monthly "Monthly" 12)
  ;; (sample by-class "By class" 12)
  (write-html raw "raw.html" "Raw data" nil t t)
  (write-csv raw "canonical.csv")
  (write-csv monthly "monthly.csv")
  ;; (write-html monthly "monthly.html" "Raw monthly summary" nil t t)
  (write-csv by-category "by-category.csv")
  (write-csv by-parentage "by-parentage.csv")
  (write-csv-with-averages by-class "by-class-with-averages.csv")
  (write-csv by-class "by-class.csv")
  (write-csv by-class-this-month "by-class-this-month.csv")
  (write-csv by-class-past-three-months "by-class-past-three-months.csv")
  (write-csv by-class-past-year "by-class-past-year.csv")
  (write-csv by-class-this-year "by-class-this-year.csv")
  (write-html by-class "by-class.html"
              "Categorised monthly summary" thresholds t t)
  (write-table by-class-past-year "summarytable" "past-year.html"
               '("Eating in" "Eating out" "Projects" "Hobbies" "Travel")
               thresholds
               nil
               nil
               nil
               t)
  (write-table by-class-past-three-months "summarytable" "past-quarter.html"
               '("Eating in" "Eating out" "Projects" "Hobbies" "Travel")
               thresholds)
  (write-html by-class-this-month "by-class-this-month.html"
              "Categorised monthly summary (last month only)" thresholds t t)
  (write-csv balance-from-statements "balance.csv")
  (write-csv monthly-balance-from-statements "monthly-balance.csv")
  (write-csv with-last-of-month "with-last-of-month.csv")
  (write-html with-last-of-month "with-last-of-month.html"
              "Categorised monthly summary" thresholds t t)
  (write-csv automatics "automatics.csv")
  (write-html automatics "automatics.html" "Automatics" thresholds t t)
  ;; (write-csv by-level-0 "by-level-0.csv")
  ;; (write-csv by-level-1 "by-level-1.csv")
  ;; (write-csv by-level-2 "by-level-2.csv")
  (write-csv by-proportions "by-proportions.csv")
  ;; (write-csv (past-months automatics 3) "past-3-months.csv")
  ;; this is the wrong data to be getting it from, it has already been clumped to the first of each month:
  ;; (write-html (by-day-of-month (past-months automatics 3)) "auto-by-day-of-month.html")
  ;; (write-html (by-day-of-week (past-months raw 3)) "by-day-of-week.html")

  ;; experimental:
  (write-csv sterling "sterling.csv")
  ;; (sample sterling-by-day "sterling by day" 12)
  (write-csv sterling-by-day "by-day-of-month.csv")
  (write-csv (row-totals sterling-by-day) "by-day-of-month-totals.csv")
  )
