
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

(defun finances-read-entry ()
  "Read the details of a transaction."
  (finances-read-completions)
  (let* ((completion-ignore-case t)
         (date (format-time-string "%F"))
         (time (format-time-string "%T"))
         (account (completing-read "Account: "
                                   account-completions
                                   nil t
                                   finances-default-account))
         (debit (y-or-n-p "Debit? "))
         (amount (read (read-from-minibuffer "Amount:" )))
         (currency (completing-read "Currency: "
                                    currency-completions
                                    nil t
                                    finances-default-currency))
         (original-amount "")
         (original-currency "")
         (category (completing-read "Category: "
                                    category-completions
                                    nil t))
         (parent "")
         (payee (completing-read "Payee:"
                                 payee-completions))
         (location "")
         (project (completing-read "Project: "
                                   project-completions))
         (note (read-from-minibuffer "Note: ")))
    (pushnew payee payee-completions :test 'string=)
    (pushnew project project-completions :test 'string=)
    (list date time
          account
          (* amount (if debit -1 1)) currency
          original-amount original-currency
          category parent
          payee location
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

(defun finances-enter (date time account amount currency original-amount original-currency category parent payee location project note)
  "Read an entry for my finances file.
The fields DATE TIME ACCOUNT AMOUNT CURRENCY ORIGINAL-AMOUNT
ORIGINAL-CURRENCY CATEGORY PARENT PAYEE LOCATION PROJECT NOTE are
as in the Financisto app."
  (interactive (finances-read-entry))
  (find-file (substitute-in-file-name "$COMMON/finances/finances.csv"))
  (save-excursion
    (goto-char (point-max))
    (unless (looking-at "^$")
      (insert "\n"))
    (let ((balance nil))
      (insert (mapconcat 'finances-entry-cell-as-string
                         (list date time
                               account amount currency
                               original-amount original-currency
                               balance
                               payee
                               category parent
                               location project
                               note)
                         ",")
              "\n"))))
