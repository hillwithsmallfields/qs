;;; csv-enter-transaction.el --- Enter a transaction into my transactions CSV file  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Sturdy

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

;; Prompt for values for a transaction, and construct and add a row for them.

;;; Code:

(require 'csv-enter-fields)

(defun transaction-enter ()
  "Read and add a transaction."
  (interactive)
  )

(defvar transactions-filename (substitute-in-file-name "$COMMON/finances/finances.csv")
  "The file holding my transactions record."

(defun transaction-add-to-file (amount payee account item category)
  "Add a transaction to the transactions file."
  (find-file transactions-filename)
  (save-excursion
    (goto-char (point-max))
    (let ((fields (make-vector 14 nil)))
      (setq fields (csv-field-add fields "account" account))
      (setq fields (csv-field-add fields "amount" amount))
      (setq fields (csv-field-add fields "note" item))
      (setq fields (csv-field-add fields "date" date))
      (setq fields (csv-field-add fields "time" time))
      (setq fields (csv-field-add fields "category" category))
      (insert (csv-field-as-string fields) "\n"))))

(provide 'csv-enter-transaction)
;;; csv-enter-transaction.el ends here
