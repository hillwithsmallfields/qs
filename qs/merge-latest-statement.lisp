(let* ((main (read-canonical "~/common/finances/finances.csv"))
       (summarised (by-day main t t))
       (latest (read-canonical incoming-statement))

       (unmatched-automatic (blank-sheet))
       (unmatched-non-automatic (blank-sheet))

       (unambiguously-matched-auto (blank-sheet))
       (unambiguously-matched-non-auto (blank-sheet))

       (ambiguously-matched-auto (blank-sheet))
       (ambiguously-matched-non-auto (blank-sheet)))
  (print "Number of original entries:" (length main))
  (print "Number of summarised entries:" (length summarised))
  (for-each-row latest this-row nil
                (let* ((amount (get this-row "amount"))
                       (direct-matches (find-amount main amount (get this-row "timestamp") 7))
                       (summarised-matches (find-amount summarised amount (get this-row "timestamp") 7))
                       (matches (append direct-matches summarised-matches))
                       (payee (get this-row "payee"))
                       (automatic (flagged-as "handelsbanken" payee "automatic")))
                  (if (> (length matches) 0)
                      (let ((found (find-by-field (get this-row "payee") matches "payee")))
                        (when verbose
                          (print "got matches on amount" amount
                                 "for payee" (get this-row "payee")
                                 "on" (get this-row "date")
                                 "by field" found)
                          (dolist (match matches)
                            (print "    payee" (get match "payee")
                                   "on" (get match "date"))))
                        (if (> (length matches) 1)
                            (if automatic
                                (add-row ambiguously-matched-auto this-row)
                              (add-row ambiguously-matched-non-auto this-row))
                          (if automatic
                              (add-row unambiguously-matched-auto this-row)
                            (add-row unambiguously-matched-non-auto this-row))))
                    (when verbose
                      (print "no matches on amount" amount
                             "for payee" payee
                             "automatic" automatic
                             "on" (get this-row "date")))
                    (if automatic
                        (add-row unmatched-automatic this-row)
                      (add-row unmatched-non-automatic this-row)))))
  (let* ((parentage-table (read-parentage-table "cats.yaml"))
         (classifiers (read-classifier "default-classes.yaml"))
         (by-classification (by-month unmatched-automatic nil nil) parentage-table classifiers t)         
         (handled-count (+ (length unmatched-automatic)
                           (length unmatched-non-automatic)
                           (length ambiguously-matched-auto)
                           (length ambiguously-matched-non-auto)
                           (length unambiguously-matched-auto)
                           (length unambiguously-matched-non-auto))))
    (print "incoming transactions:" (length latest))
    (print "unmatched automatic transactions:" (length unmatched-automatic))
    (print "unmatched non-automatic transactions:" (length unmatched-non-automatic))
    (print "ambiguously matched automatic transactions:" (length ambiguously-matched-auto))
    (print "ambiguously matched non-automatic transactions:" (length ambiguously-matched-non-auto))
    (print "unambiguously matched automatic transactions:" (length unambiguously-matched-auto))
    (print "unambiguously matched non-automatic transactions:" (length unambiguously-matched-non-auto))
    (print "total transactions handled:" handled-count)
    (print "fallen through the gaps:" (- (length latest) handled-count))

    (write-csv unmatched-automatic "unmatched-auto.csv")
    (write-csv unmatched-non-automatic "unmatched-non-auto.csv")

    (write-csv ambiguously-matched-auto "ambiguously-matched-auto.csv")
    (write-csv ambiguously-matched-non-auto "ambiguously-matched-non-auto.csv")

    (write-csv unambiguously-matched-auto "unambiguously-matched-auto.csv")
    (write-csv unambiguously-matched-non-auto "unambiguously-matched-non-auto.csv")

    (write-csv by-classification "automatics-by-classification.csv"))
  (let* ((merged (add-sheets main unmatched-automatic))
         (tracked (track merged "amount" "balance"))
         (differences (compare "discrepancy"
                               main   "amount"  "balance" "account" "Handelsbanken current account"
                               latest "balance" nil       nil       nil)))
    (print "main:" (length main))
    (print "added:" (length unmatched-automatic))
    (print "merged:" (length merged))
    (print "differences:" (length differences))
    (write-csv merged "merged.csv")
    (write-csv tracked "tracked.csv")
    (write-csv differences "differences.csv")))
