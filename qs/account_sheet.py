#!/usr/bin/python3

import csv
import os

import named_column_sheet
import qsutils

class account_sheet(named_column_sheet.named_column_sheet):
    """A sheet with a defined and ordered collection of column names.
    Each row is for a date-time."""

    def __init__(self, config, base_account=None):
        super().__init__(config, [])
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
