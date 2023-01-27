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

def file_newer_than_file(a, b): # TODO: put in library
    return os.path.getmtime(a) > os.path.getmtime(b)

def latest_file_matching(template): # TODO: put in library
    files = glob.glob(template)
    return files and sorted(files, key=os.path.getmtime)[-1]

class Finances:

    def __init__(self, facto, verbose):
        self.facto = facto
        self.verbose = verbose

    def update(self):

        """Merge new transactions from my bank statement (if I've saved a new bank statement file) and prepare CSV
        files for making into charts, and HTML for incorporating into the dashboard page."""

        config = qsutils.qsutils.load_config(self.verbose, None, None,
                                             os.path.join(self.facto.file_config('finance', 'configdir'), self.facto.config('finance', 'accounts-config')),
                                             os.path.join(self.facto.file_config('finance', 'conversions-dir'), self.facto.config('finance', 'conversions-config')))

        main_account = self.facto.file_config('finance', 'main-account')
        merge_results_dir = self.facto.file_config('finance', 'merge-results-dir')

        latest_bank_statement = latest_file_matching(self.facto.file_config('finance', 'bank-statement-template'))

        if latest_bank_statement and file_newer_than_file(latest_bank_statement, main_account):
            qsutils.qsutils.ensure_directory_present_and_empty(merge_results_dir)
            if self.verbose: print("Updating from latest bank statement", latest_bank_statement)
            # TODO: replace with new code
            # financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "merge-latest-statement.lisp")],
            #                                merge_results_dir,
            #                                config,
            #                                self.verbose,
            #                                {'incoming-statement': latest_bank_statement,
            #                                 'self.verbose': self.verbose})
            merge_results_file = os.path.join(merge_results_dir, self.facto.config('finance', 'merge-results-file'))
            if os.path.isfile(merge_results_file):
                backup.backup(main_account, self.facto.file_config('backups', 'archive'), "finances-to-%s.csv")
                shutil.copy(merge_results_file, main_account)
                if self.verbose: print("Merged bank statement into account file")
        else:
            print("Bank statement not newer than account file, so not updating")

        print("calling charter on", main_account, "with merge results in", merge_results_dir)

        # financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "chart-categories.lisp")],
        #                                self.facto.file_config('general', 'charts'),
        #                                config,
        #                                self.verbose,
        #                                {'input-file': main_account,
        #                                 'statements-file': self.facto.file_config('finance', 'accumulated-bank-statements-file'),
        #                                 'classifiers-file': self.facto.config('finance', 'budgeting-classes-file'),
        #                                 'thresholds-file': self.facto.config('finance', 'thresholds-file'),
        #                                 'self.verbose': self.verbose})

        if file_newer_than_file(main_account, self.facto.file_config('finance', 'finances-completions')):
            if self.verbose: print("updating finances completions")
            financial.list_completions.list_completions()

        return None             # TODO: return the transactions?
