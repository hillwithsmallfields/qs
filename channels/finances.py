from collections import defaultdict
import csv
import datetime
import glob
import os
import re
import sys

import yaml

import numpy as np
import pandas as pd

import dobishem
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection, linked_image
from dobishem.nested_messages import BeginAndEndMessages

import channels.panels as panels
import financial.list_completions
import financial.categorise

import qsutils

# import finutils

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

LUNCHTIME_START = datetime.time(hour=11)
LUNCHTIME_END = datetime.time(hour=14)
SUPPERTIME_START = datetime.time(hour=17)

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
    return [
        entry
        for table in tables
        for entry in table
    ]

def fixup_reload_row(raw):
    """Fix the amount type on re-reading the accumulated finances table."""
    raw['Amount'] = float(raw['Amount'])
    return raw

def spending_row_to_internal(raw):
    # This is already nearly in our internal format
    raw['Details'] = raw['Item']
    raw['Origin'] = 'Spending'
    raw['Amount'] = float(raw['Amount'])
    return raw

normalized_accounts = {}

def normalize_account(acct):
    return normalized_accounts.get(acct, "unrecognized " + acct)

def handelsbanken_row_to_internal(raw, conversions):
    item = raw.get('Details', raw.get('Narrative'))
    matched = re.search(r"([A-Z]{3}) (\d*.\d+) @ \d*.\d+ incl.commission", item)
    if matched:
        orig_curr = matched.group(1)
        orig_amount = float(matched.group(2))
    else:
        orig_curr = "GBP"
        orig_amount = None
    key = without_cruft(item).lower()
    details = conversions.get(
        key,
        {
            'Payee': "unknown",
            'Category': "unknown"
        })
    amount = float(raw.get('Money in') or "0") - float(raw.get('Money out') or "0")
    return {
        'Origin': 'Handelsbanken',
        'Date': raw['Date'],
        'Time': "00:00:01",
        'Account': normalize_account(raw['Account']),
        'Amount':  float(amount),
        'Currency': 'GBP',
        'Original_Amount':  orig_amount,
        'Original_Currency':  orig_curr,
        'Balance':  raw['Balance'],
        'Statement':  raw['Balance'],
        'Payee':  details.get('Payee', "unknown"),
        'Category':  details.get('Category', "unknown"),
        'Project': "",
        'Details': item,
        'Item': item,
        'Message': "",
    }

def monzo_row_to_internal(raw, conversions):
    date = raw['Date']
    derived_details = conversions.get(
        without_cruft(raw['Name']).lower(),
        {
            'Payee': raw['Name'],
            'Category': raw['Category'],
    })
    if derived_details['Category'] == "Eating out":
        when = datetime.time.fromisoformat(raw['Time'])
        derived_details['Category'] = (
            "Breakfast"
            if when < LUNCHTIME_START
            else ("Lunch"
                  if when < LUNCHTIME_END
                  else ("Snacks"
                        if when < SUPPERTIME_START
                        else "Supper")))
    row = {
        'Origin': 'Monzo',
        'Date':  f"{date[6:]}-{date[3:5]}-{date[0:2]}",
        'Time':  raw['Time'],
        'Account':  "Monzo",
        'Amount':  float(raw['Amount']),
        'Currency':  raw['Currency'],
        'Original_Amount':  float(raw['Local amount']),
        'Original_Currency':  raw['Local currency'],
        # 'Balance':  raw[''],
        # 'Statement':  raw[''],
        'Payee':  derived_details['Payee'],
        'Category': derived_details['Category'],
        # 'Project':  raw[''],
        'Details':  raw['Category'],
        'Item':  raw['Category'],
        'Message':  raw['Notes and #tags'],
    }
    # print("monzo", raw, "===>", row)
    return row

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

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.updated = None
        self.accumulated_bank_statements_filename = "$SYNCED/finances/handelsbanken/handelsbanken-full-new.csv"
        self.monzo_downloads_filename = "~/Downloads/Monzo Transactions - Monzo Transactions.csv"
        # manually recorded spending:
        self.spending_filename = "$SYNCED/finances/spending.csv"
        self.conversion_filename = "$SYNCED/finances/conversions.csv"
        self.hierarchy_filename = "$SYNCED/finances/cats.yaml"
        self.finances_main_filename = "$SYNCED/finances/finances-new.csv"
        self.completions_filename = "$SYNCED/var/finances-completions-new.el"
        self.dashboard_dir = os.path.expanduser("~/private_html/dashboard/")
        self.transactions = None
        self.by_categories = None
        self.known_unknowns = None
        self.categories = None
        self.parentage = financial.categorise.parentages(dobishem.storage.load("$SYNCED/finances/cats.yaml"))
        global normalized_accounts
        normalized_accounts = dobishem.storage.load("$SYNCED/finances/normalize_accounts.json")

    def name(self):
        return 'finances'

    def label(self):
        return "Spending"

    def fetch(self, verbose=False):
        """Combine my downloaded bank statements into one file."""
        dobishem.storage.combined(
            self.accumulated_bank_statements_filename,
            merge_handelsbanken_statements,
            {filename: normalize_and_filter_opening_rows
             for filename in dobishem.storage.in_modification_order("~/Downloads/Transaction*.csv")})

    def update(self, verbose=False):

        """Merge my accumulated financial data, bank statements, Monzo
        statements, and manually recorded spending into the accumulated
        file."""

        conversions = dobishem.storage.read_csv(
            self.conversion_filename,
            result_type=dict,
            key_column='Statement')

        self.transactions = qsutils.qsutils.ensure_numeric_dates(
            dobishem.storage.combined(
                self.finances_main_filename,
                finances_merger,
                {
                    self.spending_filename: spending_row_to_internal,
                    self.accumulated_bank_statements_filename: lambda row: handelsbanken_row_to_internal(row, conversions),
                    self.monzo_downloads_filename: lambda row: monzo_row_to_internal(row, conversions),
                },
                fixup_reload_row
            ))

        for row in self.transactions:
            row["Class"] = financial.categorise.nearest_ancestor_in_selection(row['Category'], self.parentage, CATEGORIES_OF_INTEREST)

        known_unknowns = [
            {'stripped': without_cruft(entry['Details'].lower())} | entry
                for entry in self.transactions
                if entry.get('Category', "unknown") == "unknown"
        ]
        dobishem.storage.write_csv(
            "$SYNCED/finances/unknown-payees.csv",
            known_unknowns,
            sort_columns=['payee'])
        self.known_unknowns = sorted(set([row['stripped'] for row in known_unknowns]))
        dobishem.storage.save("$SYNCED/finances/unknown-payees.yaml",
                              self.known_unknowns)

        if ((not os.path.exists(self.completions_filename))
            or dobishem.storage.file_newer_than_file(
                self.finances_main_filename,
                self.completions_filename)):
            financial.list_completions.list_completions()

        # eventually this will be produced inline (and cached in this file);
        # it used to come from the old Lisp part of the system
        self.by_categories = qsutils.qsutils.ensure_numeric_dates(
            financial.categorise.spread(self.transactions, "Class", "Amount"))
        self.by_categories_df = pd.DataFrame(self.by_categories)
        self.by_categories_df['Date'] = pd.to_datetime(self.by_categories_df['Date'])
        self.by_categories_df.fillna(0, inplace=True)
        self.by_categories_df.to_csv(os.path.join(self.charts_dir, "by-class.csv"))

        return self

    def prepare_page_images(self, begin_date, end_date, chart_sizes, date_suffix, verbose=False):
        """Prepare any images used by the output of the `html` method."""
        with BeginAndEndMessages("Plotting financial charts") as msgs:
            if self.by_categories_df is not None:
                qsutils.qschart.qscharts(data=self.by_categories_df,
                                         timestamp=None,
                                         columns=CATEGORIES_OF_INTEREST,
                                         begin=begin_date, end=end_date, match=None, by_day_of_week=False,
                                         outfile_template=os.path.join(
                                             self.charts_dir, "by-class-%s-%%s.png" % date_suffix),
                                         plot_param_sets=chart_sizes,
                                         messager=msgs)
                # TODO: split main file into running balances for each account (tracking as needed), take the end of each month for each account, and put them all in a file to display here (and get that shown in the resulting page)
                # qsutils.qschart.qscharts(FILECONF('finance', 'account-balances'), 'finances',
                #                        [FILECONF('finance', 'main-current-account'),
                #                         FILECONF('finance', 'main-savings-account')],
                #                        begin, end, None, False,
                #                        os.path.join(charts_dir, "balances-%s-%%s.png" % date_suffix),
                #                        chart_sizes)

    def recent_transactions_table(self, days_back):
        end_date = datetime.date.today()
        return T.div(class_='transactions_list')[
            T.table(class_='financial')[
                T.tr[T.th["Date"],
                     T.th["Amount"],
                     T.th["Payee"],
                     T.th["Category"],
                     T.th["Item"]],
                [[T.tr[T.th[transaction['Date']],
                       T.td[transaction['Amount']],
                       T.td[transaction['Payee']],
                       T.td[transaction['Category']],
                       T.td[transaction['Item']]]
                      for transaction in reversed(
                              dobishem.dates.entries_between_dates(
                                  self.transactions,
                                  dobishem.dates.back_from(end_date, None, None, days_back),
                                  end_date))]]]]

    def html(self):
        """Returns various listings of my financial transactions."""
        # TODO: spending per category per day of month/week

        full_details_file = os.path.join(self.dashboard_dir, "by-class.html")

        today = datetime.date.today()
        year_transactions = dobishem.dates.entries_between_dates(
            self.transactions,
            dobishem.dates.back_from(today, years_back=1, months_back=0, days_back=0),
            today)

        # Make the large chart that you get by clicking on the inline one:
        financial.spending_chart.spending_chart_to_file(
            year_transactions,
            key='Category', period='month',
            output=full_details_file,
            inline=True)

        return T.div[wrap_box(
            linked_image(
                charts_dir=self.dashboard_dir,
                image_name="by-class",
                label="transactions"),
            labelled_subsection("Recent transactions",
                             self.recent_transactions_table(28)),
            labelled_subsection("Spending by category",
                  T.a(class_='plainlink', href=full_details_file)[
                      financial.spending_chart.spending_chart(
                          year_transactions,
                          key='Category', period='month',
                          columns=CATEGORIES_OF_INTEREST,
                          map_to_highlights=
                          # financial.parentage.read_budgetting_classes_table(financial.finutils.BUDGETCATS)
                          financial.categorise.make_map_to_selection(
                              self.parentage,
                              CATEGORIES_OF_INTEREST))]),
            labelled_subsection("Unrecognized payees",
                                [T.p["Listed in ",
                                     os.path.expandvars("$SYNCED/finances/unknown-payees.yaml"),
                                     "; please add to ",
                                     os.path.expandvars("$SYNCED/finances/conversions.csv")],
                                 T.div(class_='transactions_list')[T.ul[
                                     [T.li[payee]
                                      for payee in self.known_unknowns]]]]),
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
