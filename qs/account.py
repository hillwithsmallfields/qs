#!/usr/bin/python3

import canonical_sheet
import csv
import datetime
import functools
import operator
import os
import payee
import qsutils
import re

class account:
    """A financial account, with the transactions that have happened on it.

    May be given a transactions argument when created, which should be
    a spreadsheet as defined in csv_sheet.py; the transactions in that
    spreadsheet will be added to this account.

    May be given a base account.  In this case, the base account
    is also used for looking at whether a transaction being added has
    already been done, so the transactions added to this account will
    be only those not in the base, and this account can then be used
    as a delta to merge with the account from which the base was
    derived.

    Iterating over an account object will yield all the payees from
    that account.

    Writing it to a CSV file will list the payees and what they were
    paid.

    """

    def __init__(self,
                 name,
                 base_account=None,
                 currency=None,
                 opening_balance=0,
                 transactions=None,
                 copy_metadata_from=None,
                 config=None,
                 origin_files=[]):
        self.name = name
        self.time_chars = 19
        self.config = config
        self.origin_files = origin_files
        tracing = config and config.get('debug', {}).get('trace', None)
        if tracing:
            self.tracing = re.compile('|'.join(tracing))
        else:
            self.tracing = None
        if copy_metadata_from:
            self.currency = copy_metadata_from.currency
            self.opening_balance = copy_metadata_from.opening_balance
            self.balance = copy_metadata_from.balance
            self.base = copy_metadata_from.base
            self.origin_files = copy_metadata_from.origin_files
        else:
            self.currency = currency
            self.opening_balance = opening_balance
            self.balance = self.opening_balance
            self.base = base_account
        self.all_transactions = {}
        self.payees = {}
        if (isinstance(transactions, canonical_sheet.canonical_sheet)
            or isinstance(transactions, account)):
            self.config = transactions.config
            self.add_sheet(transactions)

    def __str__(self):
        return ("<account " + self.name
                + " from " + '&'.join(self.origin_files)
                + " " + str(self.balance)
                + " payees " + ",".join([k if k != "" else "unknown" for k in self.payees.keys()]) + ">")

    def __iter__(self):
        self.payee_order = sorted(self.payees.keys())
        self.payee_cursor = -1    # because we pre-increment it
        return self

    def __next__(self):
        self.payee_cursor += 1
        if self.payee_cursor >= len(self.payee_order):
            raise StopIteration
        return self.payees[self.payee_order[self.payee_cursor]]

    def __len__(self):
        return len(self.payees)

    def payees_matching(self, pattern):
        """Return a list of payees with names matching a regexp."""
        compiled_pattern = re.compile(pattern)
        return {name:self.payees[name]
                for name in [payee_name
                             for payee_name in self.payees.keys()
                             if compiled_pattern.search(payee_name)]}

    def add_row_if_new(self, row):
        """Add a row to the account if it belongs to this account
        and was not already recorded.

        If it was added, return it, otherwise return None.
        """
        payee_name = row['payee']
        tracing = self.tracing and self.tracing.search(payee_name)
        row_payee = self.payees.get(payee_name, None)
        if row_payee is None:
            row_payee = payee.payee(payee_name)
            self.payees[payee_name] = row_payee
        when = row['timestamp']
        how_much = -row['amount']
        if row_payee.already_seen(when, how_much):
            if tracing:
                print("  Already seen", row)
            return None
        else:
            static_payee = (self.base.payees.get(payee_name, None)
                            if self.base
                            else None)
            if (static_payee is None
                or not static_payee.already_seen(when, how_much)):
                self.balance -= how_much
                row_payee.add_transaction(when, how_much, self)
                self.all_transactions[when] = row
                if tracing:
                    print("  Adding transaction of", row['amount'], "with", row['payee'], "at", row['timestamp'].date())
                return row
            if tracing:
                print("  Already seen", row)
            return None

    def add_sheet(self, sheet):
        """Add to this account all the rows of the given sheet
        that belong to the account and were not already recorded in it.

        Return a canonical_sheet containing only the rows that were added."""
        self.origin_files += sheet.origin_files
        added_rows = []
        if isinstance(sheet, canonical_sheet.canonical_sheet):
            for row in sheet:
                if row.get('account', None) == self.name:
                    was_new = self.add_row_if_new(row)
                    if was_new:
                        added_rows.append(was_new)
        elif isinstance(sheet, account):
            for payee in sheet:
                tracing = self.tracing and self.tracing.search(payee.name)
                if tracing:
                    print("  want to merge payments from", payee.name, "into account", self.name)
                for timestamp, row in payee:
                    seen = payee.name in self.payees and self.payees[payee.name].already_seen(timestamp, row['amount'])
                    if not seen:
                        if tracing:
                            print("    adding new transaction of", row['amount'], "with", payee.name, "at", row['timestamp'])
                        pass    # todo: how do I get a suitable result?
                    else:
                        if tracing:
                            print("    already got transaction of",
                                  row['amount'], "with", payee.name,
                                  "at", row['timestamp'].date(),
                                  "from", row['sheet'].name,
                                  "existing one is", seen['timestamp'].date(),
                                  "from", seen['sheet'].name)
        if len(added_rows) == 0:
            return None
        else:
            return canonical_sheet.canonical_sheet(None, input_sheet=added_rows)

    def already_seen(self, payee, timestamp, amount):
        """Return whether a transaction of a given amount, around a given
        time, matches any to a payee of this account.
        """
        payee = self.payees.get(payee_name, None)
        if payee is None:
            return False
        return payee.already_seen(timestamp, amount)

    def new_transactions_from(self, other_account):
        """Return which transactions in another account are new."""
        pass                    # todo: write this

    def combine_same_period_entries(self,
                                    period,
                                    time_chars=19,
                                    comment=None):
        """Produce an account based on this one, with just one entry per period
        (day, by default).

        For each payee, convert all the entries in the same period to one total.
        For all the transactions in all_transactions, do likewise.

        """
        combined = account(self.name, copy_metadata_from=self)
        # first, all the payees; these are amounts
        for name, orig_payee in self.payees.items():
            by_timestamp = orig_payee.by_timestamp
            if len(by_timestamp) <= 1:
                continue
            acc_payee = payee.payee(orig_payee.name)
            timestamps = sorted(by_timestamp.keys())
            period_start = period(timestamps[0])
            period_total = functools.reduce(operator.add,
                                            [x['amount'] for x in by_timestamp[timestamps[0]]], 0)
            for ts in timestamps[1:]:
                if period(ts) == period_start:
                    period_total += functools.reduce(operator.add,
                                                     [x['amount'] for x in by_timestamp[ts]], 0)
                else:
                    acc_payee.add_transaction(period_start, period_total,
                                              self,
                                              comment=comment)
                    period_start = period(ts)
                    period_total = functools.reduce(operator.add,
                                                    [x['amount'] for x in by_timestamp[ts]], 0)
            # Record the final period's transactions
            acc_payee.add_transaction(period_start, period_total,
                                      self,
                                      comment=comment)
            combined.payees[name] = acc_payee
        # then the overview; these are whole rows
        transactions_by_period = {}
        timestamps = sorted(self.all_transactions.keys())
        period_start = period(timestamps[0])
        period_total = self.all_transactions[timestamps[0]].get('amount', 0)
        for ts in timestamps[1:]:
            if period(ts) == period_start:
                period_total += self.all_transactions[ts].get('amount', 0)
            else:
                transactions_by_period[period_start] = period_total
                period_start = period(ts)
                period_total = self.all_transactions[ts].get('amount', 0)
            last_ts = ts
        # Record the final period's transactions
        transactions_by_period[period_start] = period_total
        combined.all_transactions = transactions_by_period
        combined.time_chars = time_chars
        return combined

    def compare_by_period(self, other):
        all_keys = (set(self.all_transactions.keys())
                    | set(other.all_transactions.keys()))
        discrepancies = {}
        for period in all_transactions:
            in_self = self.all_transactions.get(period, None)
            in_other = other.all_transactions.get(period, None)
            if in_self and in_other:
                discrepancies[period] = in_self - in_other
        return discrepancies

    def write_csv(self, filename):
        """Write a account to a CSV file.
        Each row is a payee and their payments."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            colseq = ['payee','balance','transactions']
            writer = csv.writer(outfile, colseq)
            writer.writerow(colseq)
            for payee_name in sorted(self.payees.keys()):
                payee = self.payees[payee_name]
                row = [payee_name, qsutils.trim_if_float(payee.balance), payee.transactions_string(separator='; ', time_chars=self.time_chars)]
                # round the unfortunately-represented floats
                writer.writerow(row)

# tests

import argparse

def main():
    """Tests for this module."""
    parser = qsutils.program_argparser()
    parser.add_argument("base")
    parser.add_argument("additional_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    accounts, added_row_lists = canonical_sheet.canonical_sheet(
        config, input_sheet=args.base, convert_all=True).distribute_to_accounts()
    print("Starting with", args.base, "accounts are", sorted(accounts.keys()), "and added_row_lists are", sorted(added_row_lists.keys()))
    for added_name in sorted(added_row_lists.keys()):
        print("    ", added_name, len(added_row_lists[added_name]))
    for filename in args.additional_files:
        accounts, added_row_lists = canonical_sheet.canonical_sheet(
            config, input_sheet=filename, convert_all=True).distribute_to_accounts(accounts, {})
        print("After adding", filename, "accounts are", sorted(accounts.keys()), "and added_row_lists are", sorted(added_row_lists.keys()))
        for added_name in sorted(added_row_lists.keys()):
            print("    ", added_name, len(added_row_lists[added_name]))

if __name__ == "__main__":
    main()
