;;; finances-entry.el --- enter finances tracking entries interactively  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Sturdy

;; Author: John Sturdy <jsturdy@ccsl.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Read my finances tracking data interactively, adding it to a file
;; in a format originally derived from the CSV import/export format of
;; the Android app "financisto".  Provides various forms of
;; completion, using tables generated by the accompanying program
;; list_completions.py

;;; Code:

(defvar finances-source-directory (file-name-directory load-file-name)
  "The directory this program was loaded from.")

(defvar account-completions nil
  "Completions for known accounts, loaded from finances-completions.el.")

(defvar currency-completions nil
  "Completions for known currencies, loaded from finances-completions.el.")

(defvar category-completions nil
  "Completions for known categories, loaded from finances-completions.el.")

(defvar project-completions nil
  "Completions for known projects, loaded from finances-completions.el.")

(defvar payee-completions nil
  "Completions for known payees, loaded from finances-completions.el.")

(defvar account-default-currencies nil
  "Completions for default currencies for known accounts, loaded from finances-completions.el.")

(defvar finances-completions-last-read nil
  "When the finances completions were last read.")

(defvar finances-completions-file "$SYNC/var/finances-completions.el"
  "The file to use for completions.")

(defun finances-read-completions (&optional force)
  "Read the completions tables for finances columns.
With optional FORCE, do it even if it seems unnecessary."
  (interactive)
  (let* ((completions-file-name (substitute-in-file-name finances-completions-file))
         (completions-file-date (nth 5 (file-attributes completions-file-name))))
    (when (or force
              (null finances-completions-last-read)
              (time-less-p finances-completions-last-read completions-file-date))
      (setq finances-completions-last-read completions-file-date)
      (load completions-file-name))))

(defun finances-update-completions ()
  "Update the completions tables for finances-entry.el."
  (interactive)
  (message "Updating completions...")
  (shell-command (expand-file-name "list_completions.py"
                                   finances-source-directory))
  (message "Loading updated completions...")
  (finances-read-completions t)
  (message "Updated completions"))

(defvar finances-default-account "Handelsbanken current account")
(defvar finances-default-currency "GBP")

(defun finances-categories-for-payee (payee)
  "Return the categories that have been used for a given PAYEE."
  (cons "" (cdr (assoc payee payee-completions))))

(defun completing-read-with-preloaded-history (prompt collection-var
                                                      require-match
                                                      initial-input)
  (let ((collection (symbol-value collection-var)))
    (completing-read prompt collection
                     nil require-match
                     initial-input
                     (cons collection-var
                           (1+ (position initial-input
                                         collection
                                         :test 'string=))))))

(defun finances-read-entry (&optional given-payee item given-category)
  "Read the details of a transaction.
Optional argument GIVEN-PAYEE, ITEM, and GIVEN-CATEGORY are the payee, item, and category, if pre-specified."
  (finances-read-completions)
  (let* ((completion-ignore-case t)
         (date (format-time-string "%F"))
         (time (format-time-string "%T"))
         (amount (eval (read (read-from-minibuffer "Amount:" ))))
         (debit (y-or-n-p "Debit? "))
         (account (completing-read-with-preloaded-history
                   "Account: " 'account-completions
                   t finances-default-account))
         (account-default-currency (cdr (assoc account
                                               account-default-currencies)))
         (currency (completing-read-with-preloaded-history
                    "Currency: " 'currency-completions
                    t account-default-currency))
         (converted-currency (if (equal currency account-default-currency)
                                 account-default-currency
                               (completing-read-with-preloaded-history
                                "Currency converted to: " 'currency-completions
                                t account-default-currency)))
         (converted-amount (if (equal currency
                                      account-default-currency)
                               amount
                             (read (read-from-minibuffer
                                    (format "Amount in %s: "
                                            account-default-currency)))))
         (payee (or given-payee
                    (completing-read "Payee:"
                                     payee-completions)))
         (payee-categories (finances-categories-for-payee payee))
         (category-0 (cond (given-category
                            given-category)
                           ((cdr payee-categories)
                            (completing-read (format "Category (already used for %s): "
                                                     payee)
                                             payee-categories
                                             nil t))
                           (t "")))
         (category (if (string= category-0 "")
                       (completing-read "Category (free choice): "
                                        category-completions
                                        nil t)
                     category-0))
         (project (completing-read "Project: "
                                   project-completions))
         (note (read-from-minibuffer "Note: " item)))
    ;; (pushnew payee payee-completions :test 'string=)
    (unless (assoc payee payee-completions)
      (push (list payee category)
            payee-completions))
    (pushnew project project-completions :test 'string=)
    (list date time
          account
          (* converted-amount (if debit -1 1)) account-default-currency
          (* amount (if debit -1 1)) currency
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

(defvar single-unit-currencies '("ALL"))

(defun format-currency (amount currency)
  (format (if (member currency single-unit-currencies)
              "%d"
            "%.2f")
          amount))

(defvar finances-transactions-file "$SYNC/finances/finances.csv"
  "The file holding the transactions.")

(defun finances-enter-from-shopping-list (payee item category)
  "Add a finances entry from PAYEE for ITEM in CATEGORY."
  (save-excursion
    (apply 'finances-enter (finances-read-entry payee item category))))

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
  (find-file (substitute-in-file-name finances-transactions-file))
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
                               account (format-currency amount currency) currency
                               (format-currency original-amount original-currency) original-currency
                               balance
                               0.0        ; statement
                               payee
                               category project
                               note)
                         ",")
              "\n")))
  (cons payee amount))

(defun finances-multi-enter ()
  "Read multiple entries."
  (interactive)
  (let ((first t))
    (while (or first (y-or-n-p "Another? "))
      (call-interactively 'finances-enter)
      (setq first nil))))

(defun finances-read-transfer ()
  "Read the values needed to record a fund transfer."
  (finances-read-completions)
  (let* ((completion-ignore-case t)
         (date (format-time-string "%F"))
         (time (format-time-string "%T"))
         (from-account (completing-read-with-preloaded-history
                        "From account: " 'account-completions
                        t finances-default-account))
         (from-currency (completing-read-with-preloaded-history
                         "From currency: " 'currency-completions
                         t (cdr (assoc from-account account-default-currencies))))
         (from-amount (* -1 (read (read-from-minibuffer (format "Amount in %s: " from-currency)))))
         (to-account (completing-read "To account: "
                                      account-completions
                                      nil t
                                      finances-default-account))
         (to-currency (completing-read "To currency: "
                                       currency-completions
                                       nil t
                                       (cdr (assoc to-account account-default-currencies))))
         (to-amount (if (string= from-currency to-currency)
                        (* -1 from-amount)
                      (read (read-from-minibuffer (format "Amount in %s: " to-currency)))))
         (note (read-from-minibuffer "Note: ")))
    (list date time
          from-amount from-currency from-account
          to-amount to-currency to-account note)))

(defun finances-transfer (date time
                               from-amount from-currency from-account
                               to-amount to-currency to-account
                               note)
  "Make a pair of transfer entries in my finances file.
Argument DATE is the date of the transaction; TIME the time of it.
FROM-AMOUNT is how much was transferred in FROM-CURRENCY
and TO-AMOUNT is how much it became in TO-CURRENCY.
The transfer is from FROM-ACCOUNT to TO-ACCOUNT."
  (interactive (finances-read-transfer))
  (finances-enter date time
                  from-account
                  from-amount from-currency
                  to-amount to-currency
                  "Transfer" to-account "" note)
  (finances-enter date time
                  to-account
                  to-amount to-currency
                  from-amount from-currency
                  "Transfer" from-account "" note))

(provide 'finances-entry)
;;; finances-foo.el ends here
