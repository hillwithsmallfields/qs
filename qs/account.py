import csv_sheet

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
        self.by_day = {}
        self.by_month = {}
        self.by_year = {}
        if transactions is not None:
            self.add_sheet(transactions, accumulate)

    def add_to_period(self, sheet, row,
                      period, period_str_len,
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
        row_date = sheet.get_cell(row, 'date')[:period_str_len]
        payee = sheet.get_cell(row, 'payee')
        if row_date in period:
            date_transactions = period[row_date]
            if payee in date_transactions:
                if de_duplicate:
                    amount = sheet.get_cell(row, 'amount')
                    for existing_row in date_transactions[payee]:
                        if sheet.get_cell(existing_row, 'amount') == amount:
                            return False
                if accumulate:
                    date_transactions[payee][0]['amount'] += sheet.get_cell(row, 'amount')
                else:
                    date_transactions[payee].append(row)
            else:
                date_transactions[payee] = [row]
        else:
            period[row_date] = {payee: [row]}
        return True

    def add_sheet(self, sheet,
                  de_duplicate_by_day=False,
                  de_duplicate_by_month=False,
                  de_duplicate_by_year=False,
                  accumulate=False):
        """Add all the rows of the given sheet to this account.
        No kind of de-duplication is done."""
        for row in sheet.iter():
            self.all.append(row)
            self.add_to_period(sheet, row,
                               self.by_day, 10,
                               de_duplicate=de_duplicate_by_day,
                               accumulate=accumulate)
            self.add_to_period(sheet, row,
                               self.by_month, 7,
                               de_duplicate=de_duplicate_by_month,
                               accumulate=accumulate)
            self.add_to_period(sheet, row,
                               self.by_year, 4,
                               de_duplicate=de_duplicate_by_year,
                               accumulate=accumulate)
