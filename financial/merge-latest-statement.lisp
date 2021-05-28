(defun write (sheet basename title explanation)
  (write-csv sheet (concat basename ".csv"))
  (write-html sheet (concat basename ".html")
              title
              nil nil nil
              explanation))

;; Program to merge transactions from my bank statements into my main transactions file, in which I also
;; enter transactions manually as I make them.  Some of the incoming transactions will duplicate
;; transactions already in the file, and this tries to work out which those are.
;;
;; There are a couple of complications:
;; 
;; (a) the payee names in the bank statements aren't always the same as the names I use
;;
;; (b) when I buy several things in different categories from the same payee in the same bank transaction
;; (for example, if I do my grocery shopping at a supermarket, but also buy a sandwich to eat immediately,
;; which I record as a separate category because it's something I want to track how much I do), I will make
;; multiple manual entries in the main transaction file, but there'll be only one entry in the bank
;; statement.

(let* ((main (read-canonical "~/common/finances/finances.csv"))
       ;; In case I have bought several things in different categories, in the same payment, group together
       ;; all the transactions for the same payee on the same day:
       (summarised (by-day main t t))

       ;; The bank statement that we are trying to filter to remove entries that are already covered in the
       ;; main sheet:
       (latest (read-canonical incoming-statement))

       ;; Places to collect up various types of payments of various statuses.  "automatic" means they are
       ;; flagged as from payees to whom I make automatic transactions (direct debits etc) in the
       ;; "conversions file" which maps between names I use in my manual recording of transactions, and the
       ;; names used by the bank in its statements.  Typically, automatic transactions will never be
       ;; manually entered in the main file, and so it's much likelier (in fact, almost certain) that when
       ;; they appear in the bank statement, they should be added to the main file.
       (unmatched-automatic (blank-sheet))
       (unmatched-non-automatic (blank-sheet))

       (unambiguously-matched-auto (blank-sheet))
       (unambiguously-matched-non-auto (blank-sheet))

       (ambiguously-matched-auto (blank-sheet))
       (ambiguously-matched-non-auto (blank-sheet)))
  (when verbose
    (print "Number of original entries:" (length main))
    (print "Number of summarised entries:" (length summarised)))
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
                    ;; else (= (length matches) 0)
                    (when verbose
                      (print "no matches on amount" amount
                             "for payee" payee
                             "automatic" automatic
                             "on" (get this-row "date")))
                    (if automatic
                        ;; these we are sure to want to add to the main account file
                        (add-row unmatched-automatic this-row)
                      ;; these we are quite likely to want to add to the main account file
                      (add-row unmatched-non-automatic this-row)))))
  (let* ((parentage-table (read-parentage-table "cats.yaml"))
         (classifiers (read-classifier "default-classes.yaml"))
         ;; (by-classification (by-month unmatched-automatic nil nil) parentage-table classifiers t)         
         (handled-count (+ (length unmatched-automatic)
                           (length unmatched-non-automatic)
                           (length ambiguously-matched-auto)
                           (length ambiguously-matched-non-auto)
                           (length unambiguously-matched-auto)
                           (length unambiguously-matched-non-auto))))
    (when verbose
      (print "incoming transactions:" (length latest))
      (print "unmatched automatic transactions:" (length unmatched-automatic))
      (print "unmatched non-automatic transactions:" (length unmatched-non-automatic))
      (print "ambiguously matched automatic transactions:" (length ambiguously-matched-auto))
      (print "ambiguously matched non-automatic transactions:" (length ambiguously-matched-non-auto))
      (print "unambiguously matched automatic transactions:" (length unambiguously-matched-auto))
      (print "unambiguously matched non-automatic transactions:" (length unambiguously-matched-non-auto))
      (print "total transactions handled:" handled-count)
      (print "fallen through the gaps:" (- (length latest) handled-count)))

    ;; Dump various collections for human inspection:
    (let ((show-columns '("payee" "amount")))
      (write-csv unmatched-automatic "unmatched-auto.csv")
      (write-table unmatched-automatic "financial" "unmatched-auto.html"
                   show-columns nil)
      (write-csv unmatched-non-automatic "unmatched-non-auto.csv")
      (write-table unmatched-non-automatic "financial" "unmatched-non-auto.html"
                   show-columns nil))

    (write-csv ambiguously-matched-auto "ambiguously-matched-auto.csv")
    (write-csv ambiguously-matched-non-auto "ambiguously-matched-non-auto.csv")

    (write-csv unambiguously-matched-auto "unambiguously-matched-auto.csv")
    (write-csv unambiguously-matched-non-auto "unambiguously-matched-non-auto.csv")

    ;; (write-csv by-classification "automatics-by-classification.csv")
    )
  ;; Now merge the newly-found transactions into the main collection; we'll do two versions of this, one for
  ;; the automatic unmatched transactions only, and one for all unmatched transactions.
  (let* ((merged-with-ua (add-sheets main unmatched-automatic))
         (merged-with-unmatched-all (add-sheets merged-with-ua unmatched-non-automatic))
         ;; (tracked (track merged-with-ua "amount" "balance"))

         ;; Now look for how much our balances are getting out compared with the balances from the bank
         ;; statements:
         
         ;; TODO: check this area --- is it complete?
         
         (differences-main (compare "discrepancy"
                                    main   "amount"  "balance" "account" "Handelsbanken current account"
                                    latest "balance" nil       nil       nil))
         (differences-merged-ua (compare "discrepancy"
                                         merged-with-ua   "amount"  "balance" "account" "Handelsbanken current account"
                                         latest           "balance" nil       nil       nil))
         (differences-merged-all (compare "discrepancy"
                                          merged-with-unmatched-all   "amount"  "balance" "account" "Handelsbanken current account"
                                          latest                      "balance" nil       nil       nil))
         (adjustments-main (adjustments-by-month main latest "Handelsbanken current account"))
         (adjustments-merged-ua (adjustments-by-month merged-with-ua latest "Handelsbanken current account"))
         (adjustments-merged-all (adjustments-by-month merged-with-unmatched-all latest "Handelsbanken current account")))
    (when verbose
      (print "main:" (length main))
      (print "unmatched automatic:" (length unmatched-automatic))
      (print "merged with unmatched automatic:" (length merged-with-ua))
      (print "differences against main:" (length differences-main) "average" (column-average-absolute differences-main "discrepancy"))
      (print "differences against merged ua:" (length differences-merged-ua) "average" (column-average-absolute differences-merged-ua "discrepancy"))
      (print "differences against merged all:" (length differences-merged-all) (column-average-absolute differences-merged-all "discrepancy")))
    (write-csv main "main.csv")
    (write merged-with-ua "merged-with-unmatched-automatic"
           "Merged with all unmatched automatic entries"
           "The original file, merged with the unmatched automatic entries from the incoming file.")
   (write merged-with-unmatched-all "merged-with-unmatched-all"
           "Merged with all unmatched entries"
           "The original file, merged with all unmatched entries from the incoming file.")
    ;; (write-csv tracked "tracked.csv")
    (write differences-main "differences-main"
           "Discrepancies on original data"
           "Discrepancies on original data")
    (write (occupied-columns differences-merged-ua) "differences-merged-ua"
           "Discrepancies on data merged with unmatched automatic"
           "Discrepancies on data merged with unmatched automatic")
    (write (occupied-columns differences-merged-all) "differences-merged-all"
           "Discrepancies on data merged with unmatched all"
           "Discrepancies on data merged with unmatched all")
    (write (occupied-columns (count-month-categories main)) "month-categories-main"
           "Categories by month, original"
           "Categories by month, original")
    (write (count-month-categories merged-with-ua) "month-categories-merged-with-ua"
           "Categories by month, merged with unmatched automatic"
           "Categories by month, merged with unmatched automatic")
    (write (count-month-categories merged-with-unmatched-all) "month-categories-merged-with-all"
           "Categories by month, merged with all unmatched"
           "Categories by month, merged with all unmatched")
    (write (occupied-columns adjustments-main) "adjustments-main"
           "The raw adjustments data"
           "The raw adjustments data, to match the main file to the bank statements.")
    (write (occupied-columns adjustments-merged-ua) "adjustments-merged-ua"
           "Adjustments and automatics"
           "The adjustments data, merged with unmatched automatic entries.")
    (write (occupied-columns adjustments-merged-all) "adjustments-merged-all"
           "Adjustments and unmatched all"
           "The adjustments data, merged with unmatched entries.")
    (list unmatched-automatic unmatched-non-automatic)))
