import canonical_sheet
import payee

class account:
    """A financial account, with the transactions that have happened on it.

    May be given a transactions argument when created, which should be
    a spreadsheet as defined in csv_sheet.py; the transactions in that
    spreadsheet will be added to this account.

    May be given a parent account.  In this case, the parent account
    is also used for looking at whether a transaction being added has
    already been done, so the transactions added to this account will
    be only those not in the parent, and this account can then be used
    as a delta to merge with the account from which the parent was
    derived.

    Iterating over an account object will yield all the payees from
    that account.

    """

    def __init__(self,
                 name,
                 parent_account=None,
                 currency=None,
                 opening_balance=0,
                 transactions=None,
                 accumulate=False):
        self.name = name
        self.currency = currency
        self.opening_balance = opening_balance
        self.balance = self.opening_balance
        self.all_transactions = []
        self.parent = parent_account
        self.payees = {}
        if isinstance(transactions, canonical_sheet.canonical_sheet):
            self.add_sheet(transactions, accumulate)
        elif isinstance(transactions, account) and accumulate:
            self.accumulate_sheet(transactions)

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

    def add_row(self, row):
        payee_name = row['payee']
        row_payee = self.payees.get(payee_name, None)
        if row_payee is None:
            row_payee = payee.payee(payee_name)
            self.payees[payee_name] = row_payee
        when = row['timestamp']
        how_much = -row['amount']
        if not row_payee.already_seen(when, how_much):
            static_payee = (self.parent.payees.get(payee_name, None)
                            if self.parent
                            else None)
            if (static_payee is None
                or not static_payee.already_seen(when, how_much)):
                self.balance -= how_much
                row_payee.add_transaction(when, how_much)
                self.all_transactions.append(row)

    def add_sheet(self, sheet,
                  de_duplicate=False,
                  accumulate=False):
        """Add all the rows of the given sheet to this account.

        If accumulate is given, all the row amounts for that payee for that period are summed.
        """

        for row in sheet.iter():

            # TODO: filter rows according to whether they are for this account

            self.add_row(row)

    def accumulate_sheet(self, original_sheet):
        """Fill this sheet from original_sheet grouping together all the payments on the same day."""
        # todo: check handling of first and last entries, what happens with singletons, etc
        for orig_payee in original_sheet.payees:
            acc_payee = payee.payee(orig_payee.name)
            sequence = sorted(orig_payee.by_timestamp.keys())
            if len(sequence) == 0:
                continue
            day = sequence[0].day # todo: this should be a timestamp itself
            day_total = orig_payee.by_timestamp[sequence[0]]
            for ts in sequence[1:]:
                if ts.day == day:
                    day_total += orig_payee.by_timestamp[ts]
                else:
                    acc_payee.add_transaction(day, day_total)
                    day = ts.day
                    day_total = orig_payee.by_timestamp[ts]
            # Record the final day's transactions
            acc_payee.add_transaction(day, day_total)
