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
        self.all_transactions = []
        self.parent = parent_account
        self.payees = {}
        if isinstance(transactions, canonical_sheet):
            self.add_sheet(transactions, accumulate)
        elif isinstance(transactions, account) and accumulate:
            self.accumulate_sheet(transactions)

    def add_sheet(self, sheet,
                  de_duplicate,
                  accumulate=False):
        """Add all the rows of the given sheet to this account.

        If accumulate is given, all the row amounts for that payee for that period are summed.
        """

        for row in sheet.iter():
            payee_name = row['payee']
            row_payee = self.payees.get(payee_name, None)
            if row_payee is None:
                row_payee = payee.payee(payee_name)
                self.payees[payee_name] = row_payee
            when = row['timestamp']
            how_much = row['amount']
            if not row_payee.already_seen(when, how_much):
                static_payee = (self.parent.payees.get(payee_name, None)
                                if self.parent
                                else None)
                if (static_payee is None
                    or not static_payee.already_seen(when, how_much)):
                    row_payee.add_transaction(when, how_much)
                    self.all_transactions.append(row)
