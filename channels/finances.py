from collections import defaultdict
import csv
import datetime
import glob
import os
import re
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

def without_cruft(string):
    """Returns a string trimmed of a trailing numeric part and leading spaces and stars.
    Also of a few other things.

    This gets the useful part of the annotation strings from my bank statements."""
    matched = re.match("^[^-0-9]+", string)
    remaining = (matched.group(0) if matched else string).strip()
    for cruft in ("*",          # deliberately in at the begining and the end
                  "D.DR", "B/O", "SQ", "SumUp", "ZTL*", "iZ *",
                  "*"):
        if remaining.startswith(cruft):
            remaining = remaining[len(cruft):].lstrip(" ")
    for cruft in ("REF"):
        if remaining.endswith(cruft):
            remaining = remaining[:-len(cruft)].rstrip(" ")
    return remaining.strip(" ")

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
    print("merging", len(tables), "finances tables")
    return [
        entry
        for table in tables
        for entry in table
    ]

def spending_row_to_internal(raw):
    # This is already in our internal format
    return raw

def handelsbanken_row_to_internal(raw, conversions):
    item = raw.get('Details', raw.get('Narrative'))
    matched = re.search(r"([A-Z]{3}) (\d*.\d+) @ \d*.\d+ incl.commission", item)
    if matched:
        orig_curr = matched.group(1)
        orig_amount = float(matched.group(2))
    else:
        orig_curr = "GBP"
        orig_amount = None
    details = conversions.get(
        without_cruft(item).lower(),
        {
            'payee': "unknown",
            'category': "unknown"
        })
    amount = float(raw.get('Money in') or "0") - float(raw.get('Money out') or "0")
    return {
        'Date': raw['Date'],
        'Time': "00:00:01",
        'Account': raw['Account'],
        'Amount':  amount,
        'Currency': 'GBP',
        'Original_Amount':  orig_amount,
        'Original_Currency':  orig_curr,
        'Balance':  raw['Balance'],
        'Statement':  raw['Balance'],
        'Payee':  details.get('payee', "unknown"),
        'Category':  details.get('category', "unknown"),
        'Project': "",
        'Details': item,
        'Message': "",
    }

def monzo_row_to_internal(raw, conversions):
    return {
        'Date':  raw['Date'],
        'Time':  raw['Time'],
        'Account':  "Monzo",
        'Amount':  raw['Amount'],
        'Currency':  raw['Currency'],
        'Original_Amount':  raw['Local amount'],
        'Original_Currency':  raw['Local currency'],
        # 'Balance':  raw[''],
        # 'Statement':  raw[''],
        'Payee':  raw['Name'],
        'Category':  raw['Category'], # TODO: derive from payee
        # 'Project':  raw[''],
        # 'Details':  raw[''],
        'Message':  raw['Notes and #tags'],
    }

def normalize_and_filter_opening_rows(raw):
    details = raw.get('Details', raw.get('Narrative'))
    return (None
            if (details.startswith("Opening Balance")
                or details.startswith("D.DR PAYPAL PAYMENT"))
            else {
                    'Account': raw['Account'],
                    'Date': raw.get('Date', raw.get('Value Date')),
                    'Details': details,
                    'Money in': raw.get('Money in', raw.get('Cr Amount')),
                    'Money out': raw.get('Money out', raw.get('Dr Amount')),
                    'Balance': raw['Balance'],
            }
            )

class FinancesPanel(panels.DashboardPanel):

    def __init__(self):
        self.updated = None
        self.accumulated_bank_statements_filename = "$SYNCED/finances/handelsbanken/handelsbanken-full-new.csv"

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

        dobishem.storage.write_csv(
            "$SYNCED/finances/unknown-payees.csv",
            [
                entry
                for entry in dobishem.storage.combined(
                    "$SYNCED/finances/finances-new.csv",
                    finances_merger,
                    {
                        "$SYNCED/finances/spending.csv": spending_row_to_internal,
                        self.accumulated_bank_statements_filename: lambda row: handelsbanken_row_to_internal(row, conversions),
                        "~/Downloads/Monzo Transactions - Monzo Transactions.csv": lambda row: monzo_row_to_internal(row, conversions),
                    }
                )
                if entry.get('category', "unknown") == "unknown"
            ],
            sort_columns=['payee'])

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
