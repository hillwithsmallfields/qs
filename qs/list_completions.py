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
    with open(args.output or os.path.expandvars("$COMMON/var/finances-completions.el"), 'w') as outstream:
        outstream.write("(setq ")
        for column_name in (args.columns if len(args.columns) > 0 else default_columns):
            outstream.write("  " + column_name + '''-completions '("''')
            outstream.write('"\n    "'.join(sorted(set([row[column_name] for row in rows if row[column_name]]))))
            outstream.write('")\n')
        outstream.write("  payee-completions '(")
        for payee in sorted(payees.keys()):
            outstream.write('    ("' + payee + '"'
                            + (' "' + '" "'.join(sorted(set(payees[payee]))) + '"' if payees[payee] else '')
                            + ')\n')
        outstream.write("  )\n")
        outstream.write(')\n')

if __name__ == "__main__":
    main()
    
