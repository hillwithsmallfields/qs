
(defvar finances-completions-last-read nil
  "When the finances completions were last read.")

(defun finances-read-completions (&optional force)
  "Read the completions tables for finances columns.
With optional FORCE, do it even if it seems unnecessary."
  (interactive)
  (let* ((completions-file-name (substitute-in-file-name "$COMMON/var/finances-completions.el"))
         (completions-file-date (nth 5 (file-attributes completions-file-name))))
    (when (or force
              (null finances-completions-last-read)
              (time-less-p finances-completions-last-read completions-file-date))
      (setq finances-completions-last-read completions-file-date)
      (load completions-file-name))))

(defvar finances-default-account "Handelsbanken current account")
(defvar finances-default-currency "GBP")

(defun finances-categories-for-payee (payee)
  "Return the categories that have been used for a given PAYEE."
  (cons "" (cdr (assoc payee payee-completions))))

(defun finances-read-entry ()
  "Read the details of a transaction."
  (finances-read-completions)
  (let* ((completion-ignore-case t)
         (date (format-time-string "%F"))
         (time (format-time-string "%T"))
         (amount (read (read-from-minibuffer "Amount:" )))
         (debit (y-or-n-p "Debit? "))
         (currency (completing-read "Currency: "
                                    currency-completions
                                    nil t
                                    finances-default-currency))
         (account (completing-read "Account: "
                                   account-completions
                                   nil t
                                   finances-default-account))
         (original-amount "")
         (original-currency "")
         (payee (completing-read "Payee:"
                                 payee-completions))
         (payee-categories (finances-categories-for-payee payee))
         (category-0 (if (cdr payee-categories)
                         (completing-read (format "Category (already used for %s): " payee)
                                          payee-categories
                                          nil t)
                       ""))
         (category (if (string= category-0 "")
                       (completing-read "Category (free choice): "
                                        category-completions
                                        nil t)
                     category-0))
         (project (completing-read "Project: "
                                   project-completions))
         (note (read-from-minibuffer "Note: ")))
    ;; (pushnew payee payee-completions :test 'string=)
    (unless (assoc payee payee-completions)
      (push (list payee category)
            payee-completions))
    (pushnew project project-completions :test 'string=)
    (list date time
          account
          (* amount (if debit -1 1)) currency
          original-amount original-currency
          category payee
          project note)))

(defun finances-entry-cell-as-string (cell-value)
  (cond
   ((stringp cell-value)
    (if (position ?, cell-value)
        (concat "\"" cell-value "\"")
      cell-value))
   ((numberp cell-value)
    (format "%g" cell-value))
   (t cell-value)))

(defun adjust-date-time (date time)
  "Adjust the given DATE and TIME to avoid clashing with any already in the buffer."
  (while (save-excursion
           (goto-char (point-min))
           (search-forward (format "%s,%s," date time) (point-max) t))
    (let ((date-time (time-add (parse-iso8601-time-string (format "%sT%s" date time))
                               1)))
      (setq date (format-time-string "%F" date-time)
            time (format-time-string "%T" date-time))))
  (cons date time))

(defun finances-enter (date time
                            account amount currency
                            original-amount original-currency
                            category payee
                            project note)
  "Make an entry for my finances file.
The fields DATE TIME ACCOUNT AMOUNT CURRENCY ORIGINAL-AMOUNT
ORIGINAL-CURRENCY CATEGORY PARENT PAYEE LOCATION PROJECT NOTE are
as in the Financisto app."
  (interactive (finances-read-entry))
  (find-file (substitute-in-file-name "$COMMON/finances/finances.csv"))
  (save-excursion
    (goto-char (point-max))
    (unless (looking-at "^$")
      (insert "\n"))
    (let* ((balance nil)
           (adjusted-date-time (adjust-date-time date time))
           (adjusted-date (car adjusted-date-time))
           (adjusted-time (cdr adjusted-date-time)))
      (insert (mapconcat 'finances-entry-cell-as-string
                         (list adjusted-date adjusted-time
                               account amount currency
                               original-amount original-currency
                               balance
                               payee
                               category project
                               note)
                         ",")
              "\n"))))

(defun finances-read-transfer ()
  "Read the values needed to record a fund transfer."
  (finances-read-completions)
  (let* ((completion-ignore-case t)
         (date (format-time-string "%F"))
         (time (format-time-string "%T"))
         (from-amount (read (read-from-minibuffer "Amount in source currency: ")))
         (from-currency (completing-read "From currency: "
                                         currency-completions
                                         nil t
                                         finances-default-currency))
         (from-account (completing-read "From account: "
                                        account-completions
                                        nil t
                                        finances-default-account))
         (to-currency (completing-read "To currency: "
                                       currency-completions
                                       nil t
                                       finances-default-currency))
         (to-amount (if (string= from-currency to-currency)
                        from-amount
                      (read (read-from-minibuffer "Amount in destination currency: "))))
         (to-account (completing-read "To account: "
                                      account-completions
                                      nil t
                                      finances-default-account)))
    (list date time
          from-amount from-currency from-account
          to-amount to-currency to-account)))

(defun finances-transfer (date time from-amount from-currency from-account to-amount to-currency to-account)
  "Make a pair of transfer entries in my finances file.
Argument DATE is the date of the transaction; TIME the time of it.
FROM-AMOUNT is how much was transferred in FROM-CURRENCY
and TO-AMOUNT is how much it became in TO-CURRENCY.
The transfer is from FROM-ACCOUNT to TO-ACCOUNT."
  (interactive (finances-read-transfer))
  (finances-enter date time
                  from-account from-amount from-currency
                  from-amount from-currency
                  "Transfer" to-account "" "")
  (finances-enter date time
                  to-account to-amount from-currency
                  from-amount from-currency "Transfer" from-account "" ""))
