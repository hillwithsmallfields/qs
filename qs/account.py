import canonical_sheet
import payee

class account:
    """A financial account, with the transactions that have happened on it.
    May be given a transactions argument when created, which should be a
    spreadsheet as defined in csv_sheet.py.
    """

    def __init__(self,
                 name,
                 currency=None,
                 opening_balance=0,
                 transactions=None, accumulate=False):
        self.name = name
        self.currency = currency
        self.opening_balance = opening_balance
        self.all = []
        self.payees = {}
        if isinstance(transactions, canonical_sheet):
            self.add_sheet(transactions, accumulate)
        elif isinstance(transactions, account) and accumulate:
            self.accumulate_sheet(transactions)

    def add_to_period(self, sheet, row,
                      by_period, period_str_len,
                      de_duplicate=False,
                      accumulate=False):
        """Add a row from a sheet to a period in an account.
        The period should be one of self.by_day etc from the account.
        period_str_len is how much of the isodate string to use to get
        that kind of period.
        If de_duplicate is given, if there is an existing row for the same
        date and the same payee, this row is not added.
        If accumulate is given, if there is an existing row for the same
        date and the same payee, this row is merged into that one."""
        row_date = row['date'][:period_str_len]
        payee = row['payee']
        if row_date in by_period:
            date_transactions = by_period[row_date]
            if payee in date_transactions:
                if de_duplicate:
                    amount = row['amount']
                    for existing_row in date_transactions[payee]:
                        if sheet.get_cell(existing_row, 'amount') == amount:
                            return False
                if accumulate:
                    date_transactions[payee][0]['amount'] += row['amount']
                else:
                    date_transactions[payee].append(row)
            else:
                date_transactions[payee] = [row]
        else:
            by_period[row_date] = {payee: [row]}
        return True

    def add_sheet(self, sheet,
                  de_duplicate,
                  accumulate=False):
        """Add all the rows of the given sheet to this account.

        If de_duplicate is 'day', 'month' or 'year', and the payee and
        the amount are the same as an existing entry for that time
        period, a row will be treated as already present, and not
        added.

        If accumulate is given, all the row amounts for that payee for that period are summed.
        """

        for row in sheet.iter():
            payee_name = row['payee']
            row_payee = self.payees.get(payee_name, None)
            if row_payee is None:
                row_payee = payee.payee(payee_name)
                self.payees[payee_name] = row_payee
            if row_payee.add_transaction(row['timestamp'], row['amount']):
                self.all.append(row)
