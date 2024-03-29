#!/usr/bin/python3

import argparse
import csv
import os.path

default_columns = ['Account', 'Currency', 'Category', 'Project']

def list_completions(input_file=os.path.expandvars("$SYNCED/finances/finances-new.csv"),
                     output_file=os.path.expandvars("$SYNCED/var/finances-completions-new.el"),
                     columns=default_columns):
    with open(input_file) as instream:
        rows = [row for row in csv.DictReader(instream)]
        payees = {p: [row['Category']
                      for row in rows
                      if row['Payee'] == p] for p in set([row['Payee']
                                                          for row in rows
                                                          if row['Payee']])}
        currencies_by_account = {}
        for row in rows:
            account = row['Account']
            currency = row['Currency']
            if account not in currencies_by_account:
                currencies_by_account[account] = {}
            currencies_by_account[account][currency] = currencies_by_account[account].get(currency, 0) + 1
        account_currencies = {}
        for account, currency_counts in currencies_by_account.items():
            highest_count = 0
            highest_currency = None
            for currency, count in currency_counts.items():
                if count > highest_count:
                    highest_currency = currency
                    highest_count = count
                account_currencies[account] = highest_currency
        with open(output_file , 'w') as outstream:
            outstream.write("(setq\n")
            for column_name in columns:
                outstream.write("  " + column_name + '''-completions\n  '("''')
                outstream.write('"\n    "'.join(sorted(set([row[column_name] for row in rows if row[column_name]]))))
                outstream.write('")\n')
            outstream.write("  payee-completions\n  '(")
            outstream.write("\n    ".join(['("' + payee + '"'
                                           + (' "' + '" "'.join(sorted(set(payees[payee]))) + '"' if payees[payee] else '')
                                           + ')'
                                           for payee in sorted(payees.keys())]))
            outstream.write(")\n")
            outstream.write("  account-default-currencies\n  '(")
            outstream.write("\n    ".join(['("%s" . "%s")' % (account, account_currencies[account])
                                           for account in sorted(account_currencies.keys())]))
            outstream.write(")")
            outstream.write(')\n')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", "-o",
                        help="""File to produce.""")
    parser.add_argument("--input", "-i",
                        help="""File to read.""")
    parser.add_argument("columns",
                        nargs='*',
                        help="""Column to collect.""")
    args = parser.parse_args()
    list_completions(args.input, args.output, args.columns)

if __name__ == "__main__":
    main()
