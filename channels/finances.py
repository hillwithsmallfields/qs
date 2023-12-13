from collections import defaultdict
import csv
import datetime
import glob
import os
import sys

import yaml

import dobishem.dates
import dobishem.storage

import channels.panels as panels

# import finutils

def recent_transactions_table(filename, days_back):
    start_date = dobishem.dates.back_from(datetime.date.today(), None, None, days_back)
    with open(filename) as instream: # TODO: pass the latest transactions in memory
        recent_transactions = [transaction
                               for transaction in csv.DictReader(instream)
                               if datetime.date.fromisoformat(transaction['date']) >= start_date]
    return T.div(class_='transactions_list')[
        T.table(class_='financial')[
            T.tr[T.th["Date"],T.th["Amount"],T.th["Payee"],T.th["Category"],T.th["Item"]],
            [[T.tr[T.th[transaction['date']],
                   T.td[transaction['amount']],
                   T.td[transaction['payee']],
                   T.td[transaction['category']],
                   T.td[transaction['item']]]
                  for transaction in reversed(recent_transactions)]]]]

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

def similar_date(a, b):
    return abs((a - b).days) <= 3

def already_got(entry, seen):
    """Return whether we have already seen an entry.

    This handles the overlap between bank statements, allowing for the
    date of an entry to vary between the statements.
    """
    entry_date = datetime.date.fromisoformat(entry.get('Value Date', entry.get('Date')))
    if entry_date is None:
        return False
    entry_record = (entry.get('Item'),
                    entry.get('Money out'),
                    entry.get('Money in'))
    result = False
    if entry_record in seen:
        for possible in seen[entry_record]:
            if similar_date(entry_date, possible):
                result = True
                break
    seen[entry_record].add(entry_date)
    return result

def merge_handelsbanken_statements(statements):
    """Merge some bank statements.

    They may overlap, and the dates may change between the overlapping
    copies.
    """
    seen = defaultdict(set)
    return [
        entry
        for statement in statements
        for entry in statement
        if not already_got(entry, seen)
    ]

def finances_merger(tables):
    print("merging", len(tables), " finances tables")
    return None                 # TODO

def spending_row_to_internal(raw):
    return raw

def handelsbanken_row_to_internal(raw):
    return raw                 # TODO

def monzo_row_to_internal(raw):
    return raw                  # TODO

def normalize_and_filter_opening_rows(raw):
    details = raw.get('Details', raw.get('Narrative'))
    return (None
            if (details.startswith("Opening Balance")
                or details.startswith("D.DR PAYPAL PAYMENT"))
            else {
                    'Account': raw['Account'],
                    'Date': raw.get('Date', raw.get('Value Date')),
                    'Item': details,
                    'Money in': raw.get('Money in', raw.get('Cr Amount')),
                    'Money out': raw.get('Money out', raw.get('Dr Amount')),
                    'Balance': raw['Balance'],
            }
            )

class FinancesPanel(panels.DashboardPanel):

    def __init__(self):
        self.updated = None
        self.accumulated_bank_statements_filename = "$SYNCED/finances/handelsbanken/handelsbanken-full.csv"

    def name(self):
        return 'finances'

    def fetch(self):
        """Combine my downloaded bank statements into one file."""
        dobishem.storage.combined(
            self.accumulated_bank_statements_filename,
            merge_handelsbanken_statements,
            {filename: normalize_and_filter_opening_rows
             for filename in dobishem.storage.in_modification_order("~/Downloads/Transaction*.csv")})

    def update(self):

        """Merge my accumulated financial data, bank statements, Monzo
        statements, and manually recorded spending into the accumulated
        file."""

        conversions = dobishem.storage.read_csv(
            "$SYNCED/finances/conversions.csv",
            result_type=dict,
            key_column='statement')

        transactions = dobishem.storage.combined(
            "$SYNCED/finances/finances.csv",
            finances_merger,
            {
                "$SYNCED/finances/spending.csv": spending_row_to_internal,
                self.accumulated_bank_statements_filename: handelsbanken_row_to_internal,
                "~/Downloads/Monzo Transactions - Monzo Transactions.csv": monzo_row_to_internal,
            }
        )

        merge_results_file = os.path.join(merge_results_dir, self.facto.config('finance', 'merge-results-file'))

        all_transactions = merge_bank_to_main.merge_bank_to_main(
            finutils.read_transactions(self.facto.file_config('finance', 'main-account-file')),
            bank_statement,
            conversions)

        unknown_payees = find_unknown_payees.find_unknown_payees(bank_statement, conversions)

        # print("Unknown payees are:", sorted(list(set(unknown_payees.keys()))))

        finutils.write_csv([{'statement': k.lower(), 'payee': k.title()}
                            for k, row in unknown_payees.items()],
                           ['statement', 'payee', 'category', 'flags'],
                           self.facto.file_config('finance', 'for-categorization'),
                           lambda r: r['statement'])

        finutils.write_csv(all_transactions,
                           finutils.MAIN_HEADERS,
                           merge_results_file,
                           lambda r: (r['date'], r['time'], r'[payee'))

        if os.path.isfile(merge_results_file):
            print("written merged finances to", merge_results_file)
            # backup.backup(main_account, self.facto.file_config('backups', 'archive'), "finances-to-%s.csv")
            # shutil.copy(merge_results_file, main_account)
            # if verbose: print("Merged bank statement into account file")

        if file_newer_than_file(main_account, self.facto.file_config('finance', 'finances-completions')):
            if verbose: print("updating finances completions")
            financial.list_completions.list_completions()

        return self

    def html(self):
        """Returns various listings of my financial transactions."""
        # TODO: spending per category per day of month/week

        some_columns = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

        full_details_file = "by-class.html" # todo: place this in a specific directory

        account_file = self.facto.file_config('finance', 'main-account')

        # Make the large chart that you get my clicking on the inline one:
        financial.spending_chart.spending_chart_to_file(
            # financial.finutils.read_transactions(self.facto.file_config('finance', 'main-account')),
            account_file,
            key='category', period='month',
            output=full_details_file,
            inline=True)

        return T.div[panels.wrap_box(
            panels.linked_image("by-class", "transactions"),
            T.div[T.h3["Recent transactions"],
                  recent_transactions_table(account_file, 14)],
            T.div[T.h3["Spending by category"],
                  T.a(class_='plainlink', href=full_details_file)[
                      financial.spending_chart.spending_chart(
                          financial.finutils.read_transactions(
                              account_file,
                              datetime.date.today() - datetime.timedelta(days=365)),
                          key='category', period='month',
                          columns=some_columns,
                          map_to_highlights = financial.parentage.read_budgetting_classes_table(
                              financial.finutils.BUDGETCATS))
                  ]],
            # T.div[T.h3["Automatic Spending by day of month"],
            #       untemplate.safe_unicode(qsutils.html_pages.file_contents(os.path.join(self.facto.file_config('finance', 'merge-results-dir'),
            #                                                          "auto-by-day-of-month.html")))],
            # T.div[T.h3["Spending by day of week"],
            #       untemplate.safe_unicode(qsutils.html_pages.file_contents(os.path.join(self.facto.file_config('finance', 'merge-results-dir'),
            #                                                          "by-day-of-week.html")))],
            # T.div[T.h3["Unmatched automatic transactions"],
            #       untemplate.safe_unicode(qsutils.html_pages.file_contents(os.path.join(self.facto.file_config('finance', 'merge-results-dir'),
            #                                                          "unmatched-auto.html")))],
            # T.div[T.h3["Unmatched non-automatic transactions"],
            #       untemplate.safe_unicode(qsutils.html_pages.file_contents(os.path.join(self.facto.file_config('finance', 'merge-results-dir'),
            #                                                          "unmatched-non-auto.html")))]
        )]
