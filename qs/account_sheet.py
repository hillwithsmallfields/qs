#!/usr/bin/python3

import csv
import os

import base_sheet
import qsutils

class account_sheet(base_sheet.base_sheet):
    """A sheet converted from an account.
    Each row is a date, and each column a payee."""

    def __init__(self, config, base_account=None):
        super().__init__(config)
        self.column_names = []
        self.rows = {}
        if base_account:
            self.column_names = sorted(base_account.payees)
            for payee_name in sorted(base_account.payees):
                transactions_for_payee = base_account.payees[payee_name]
                for transaction_date in sorted(transactions_for_payee.by_timestamp):
                    if transaction_date not in self.rows:
                        self.rows[transaction_date] = {}
                    self.rows[transaction_date][payee_name] = \
                        sum([transaction['amount']
                             for transaction in transactions_for_payee.by_timestamp[transaction_date]])

    def write_csv(self, filename):
        """Write an account sheet to a file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            writer = csv.writer(outfile)
            writer.writerow(['Date'] + self.column_names)
            for date in sorted(self.rows):
                row_data = self.rows[date]
                writer.writerow([date] + [row_data.get(n, '') for n in self.column_names])
