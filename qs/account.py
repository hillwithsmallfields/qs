import canonical_sheet
import payee

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

    """

    def __init__(self,
                 name,
                 base_account=None,
                 currency=None,
                 opening_balance=0,
                 transactions=None,
                 copy_metadata_from=None):
        self.name = name
        if copy_metadata_from:
            self.currency = copy_metadata_from.currency
            self.opening_balance = copy_metadata_from.opening_balance
            self.balance = copy_metadata_from.balance
            self.base = copy_metadata_from.base
        else:
            self.currency = currency
            self.opening_balance = opening_balance
            self.balance = self.opening_balance
            self.base = base_account
        self.all_transactions = {}
        self.payees = {}
        if isinstance(transactions, canonical_sheet.canonical_sheet):
            self.add_sheet(transactions)

    def __str__(self):
        return ("<account " + self.name
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

    def add_row_if_new(self, row):
        payee_name = row['payee']
        row_payee = self.payees.get(payee_name, None)
        if row_payee is None:
            row_payee = payee.payee(payee_name)
            self.payees[payee_name] = row_payee
        when = row['timestamp']
        how_much = -row['amount']
        if not row_payee.already_seen(when, how_much):
            static_payee = (self.base.payees.get(payee_name, None)
                            if self.base
                            else None)
            if (static_payee is None
                or not static_payee.already_seen(when, how_much)):
                self.balance -= how_much
                row_payee.add_transaction(when, how_much)
                self.all_transactions[when] = row

    def add_sheet(self, sheet):
        """Add all the rows of the given sheet to this account."""
        for row in sheet.iter():
            if row.get('account', None) == self.name:
                self.add_row_if_new(row)

    def combine_same_period_entries(self, period=lambda dt: dt.day):
        """Produce an account base on this one, with just one entry per period
        (day, by default).

        For each payee, convert all the entries in the same period to one total.
        For all the transactions in all_transactions, do likewise.

        """
        combined = account(self.name, copy_metadata_from=self)
        for name, orig_payee in self.payees.items():
            by_timestamp = orig_payee.by_timestamp
            if len(by_timestamp) <= 1:
                continue
            acc_payee = payee.payee(orig_payee.name)
            timestamps = sorted(by_timestamp.keys())
            period = period(timestamps[0])
            period_total = by_timestamp[timestamps[0]]
            for ts in timestamps[1:]:
                if period(ts) == period:
                    period_total += by_timestamp[ts]
                else:
                    acc_payee.add_transaction(period, period_total)
                    period = period(ts)
                    period_total = by_timestamp[ts]
            # Record the final period's transactions
            acc_payee.add_transaction(period, period_total)
            combined.payees[name] = acc_payee
        transactions_by_period = {}
        timestamps = sorted(self.all_transactions.keys())
        period = period(timestamps[0])
        period_total = self.all_transactions[timestamps[0]]
        for ts in timestamps[1:]:
            if period(ts) == period:
                period_total += self.all_transactions[ts]
            else:
                transactions_by_period[period] = period_total
                period = period(ts)
                period_total = self.all_transactions[ts]
        # Record the final period's transactions
        transactions_by_period[period] = period_total
        combined.all_transactions = transactions_by_period
        return combined

    def compare_by_period(self, other):
        all_keys = set(self.all_transactions.keys()) | set(other.all_transactions.keys())
        discrepancies = {}
        for period in all_transactions:
            in_self = self.all_transactions.get(period, None)
            in_other = other.all_transactions.get(period, None)
            if in_self and in_other:
                discrepancies[period] = in_self - in_other
        return discrepancies
