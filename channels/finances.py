import csv
import datetime
import glob
import os
import sys

import yaml

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

# other parts of this project group:
ensure_in_path(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import qsutils
import financial.list_completions

import merge_bank_downloads
import merge_bank_to_main
import find_unknown_payees

source_dir = os.path.dirname(os.path.realpath(__file__))

import panels

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

ensure_in_path(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

def file_newer_than_file(a, b): # TODO: put in library
    return os.path.getmtime(a) > os.path.getmtime(b)

def latest_file_matching(template): # TODO: put in library
    files = glob.glob(template)
    return files and sorted(files, key=os.path.getmtime)[-1]

def recent_transactions_table(filename, days_back):
    start_date = qsutils.qsutils.back_from(datetime.date.today(), None, None, days_back)
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

class Finances:

    def __init__(self, facto):
        self.facto = facto
        self.updated = None

    def update(self, read_external, verbose):

        """Merge transactions from my bank statement (if I've saved a new bank statement file) and prepare CSV
        files for making into charts, and HTML for incorporating into the dashboard page."""

        config = qsutils.qsutils.load_config(
            verbose, None, None,
            os.path.join(self.facto.file_config('finance', 'configdir'),
                         self.facto.config('finance', 'accounts-config')),
            os.path.join(self.facto.file_config('finance', 'conversions-dir'),
                         self.facto.config('finance', 'conversions-config')))

        main_account = self.facto.file_config('finance', 'main-account')
        merge_results_dir = self.facto.file_config('finance', 'merge-results-dir')

        if verbose: print("Updating from latest bank statements")

        bank_file = self.facto.file_config('finance', 'accumulated-bank-statements-file')
        bank_statement = merge_bank_download_files(bank_file,
                                                   self.facto.config('finance', 'main-account-number'),
                                                   bank_file,
                                                   self.facto.config('finance', 'bank-statement-template'))

        conversions = finutils.read_conversions(self.facto.file_config('conversions-dir', 'conversions-config'))

        merge_results_file = os.path.join(merge_results_dir, self.facto.config('finance', 'merge-results-file'))

        all_transactions = merge_bank_to_main(finutils.read_transactions(self.facto.file_config('finance', 'main-account-file')),
                                              bank_statement,
                                              finutils.read_conversions(conversions))

        unknown_payees = find_unknown_payees.find_unknown_payees(bank_statement, conversions)

        print("Unknown payees are:", unknown_payees)

        finutils.write_csv(all_transactions,
                           finutils.MAIN_HEADERS,
                           merge_results_file,
                           lambda r: (r['date'], r['time'], r'[payee'))

        if os.path.isfile(merge_results_file):
            backup.backup(main_account, self.facto.file_config('backups', 'archive'), "finances-to-%s.csv")
            shutil.copy(merge_results_file, main_account)
            if verbose: print("Merged bank statement into account file")

        print("calling charter on", main_account, "with merge results in", merge_results_dir)

        # financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "chart-categories.lisp")],
        #                                self.facto.file_config('general', 'charts'),
        #                                config,
        #                                verbose,
        #                                {'input-file': main_account,
        #                                 'statements-file': self.facto.file_config('finance', 'accumulated-bank-statements-file'),
        #                                 'classifiers-file': self.facto.config('finance', 'budgeting-classes-file'),
        #                                 'thresholds-file': self.facto.config('finance', 'thresholds-file'),
        #                                 'verbose': verbose})

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
