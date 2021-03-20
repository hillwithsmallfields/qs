#!/usr/bin/python3

import argparse
import csv
import os.path

default_columns = ['account', 'currency', 'category', 'project']

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
    with open(args.input or os.path.expandvars("$COMMON/finances/finances.csv")) as instream:
        rows = [row for row in csv.DictReader(instream)]
        payees = {p: [row['category']
                      for row in rows
                      if row['payee'] == p] for p in set([row['payee']
                                                          for row in rows
                                                          if row['payee']])}
        currencies_by_account = {}
        for row in rows:
            account = row['account']
            currency = row['currency']
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
        with open(args.output or os.path.expandvars("$COMMON/var/finances-completions.el"), 'w') as outstream:
            outstream.write("(setq\n")
            for column_name in (args.columns if len(args.columns) > 0 else default_columns):
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

if __name__ == "__main__":
    main()
