#!/usr/bin/python

# Program to split accounts into a separate column per category or payee

import argparse
import csv
import os
import qsutils

# See notes in finconv.py for config file format

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def finsplit_setup(args, config, input_format):
    in_columns = input_format['columns']
    print "in_columns are", in_columns
    return ['date'], {'split_by': input_format['columns']['payee'
                                                          if args.payee
                                                          else 'category'],
                      'verticals': set(()),
                      'date_column_name': in_columns['date'],
                      'amount_column': in_columns['amount'][args.account]}

def finsplit_row(timestamp, row, output_rows, scratch):
    decider = row[scratch['split_by']]
    scratch['verticals'].add(decider)
    row_date = row[scratch['date_column_name']]
    amount = row[scratch['amount_column']]
    output_row = {'date': row_date,
                  decider: amount}
    output_rows[row_date] = output_row

def finsplit_tidyup(columns, rows, scratch):
    return columns + sorted(scratch['verticals']), rows

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append')
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')
    parser.add_argument("-o", "--output")
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("-a", "--account",
                        help="The account column to split.")
    split_by = parser.add_mutually_exclusive_group(required=True)
    split_by.add_argument("-C", "--category", action='store_true')
    split_by.add_argument("-P", "--payee", action='store_true')
    parser.add_argument("input_file")

    args = parser.parse_args()

    print "args are", args

    config_files = ([DEFAULT_CONF]
                    if os.path.exists(DEFAULT_CONF) and not args.no_default_config
                    else [])

    if args.config:
        config_files += args.config

    config = qsutils.load_config(args.verbose, *config_files)

    qsutils.process_fin_csv(args, config, finsplit_setup, finsplit_row, finsplit_tidyup)

if __name__ == "__main__":
    main()
