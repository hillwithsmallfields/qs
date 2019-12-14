#!/usr/bin/python

# Program to sum each day's transactions per payee in finance spreadsheets

import argparse
import csv
import os
import qsutils
import yaml

# See notes in finconv.py for config file format

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append',
                        help="""Extra config file (may be given multiple times).""")
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-f", "--format",
                        default='combined')
    parser.add_argument("-o", "--output")
    parser.add_argument("-k", "--key",
                        help="""Key to group by,""",
                        default="payee")
    period_size = parser.add_mutually_exclusive_group()
    period_size.add_argument("-m", "--monthly",
                             help="""Group by months instead of days.""")
    period_size.add_argument("-y", "--yearly",
                             help="""Group by years instead of days.""")
    parser.add_argument("input_file")
    args = parser.parse_args()

    config = qsutils.load_config(args.verbose,
                                 qsutils.DEFAULT_CONF if not args.no_default_config else None,
                                 *args.config or ())

    time_periods = {}
    date_size = 10
    if args.monthly:
        date_size = 7
    elif args.yearly:
        date_size = 4

    input_file_name = os.path.expanduser(os.path.expandvars(args.input_file))
    with open(input_file_name) as infile:
        input_format = qsutils.deduce_format(sample_row, config['formats']) or config['formats'][args.format]
        print "input format for", input_file_name, "is", input_format
        in_columns = input_format['columns']
        date_column = in_columns['date']
        key_column = in_columns[args.key]
        account_columns = input_format['accounts']
        for row in csv.DictReader(infile):
            date = row[date_column][:date_size]
            period_data = time_periods[date] = time_periods.get(date, {date_column: date})
            grouping_key = row.get(key_column, "unknown")
            payee_accounts = period_data[grouping_key] = period_data.get(grouping_key, {})
            for account_name in account_columns:
                payee_accounts[account_name] = (payee_accounts.get(account_name, 0.0)
                                                + float(row.get(account_name, 0.0)))

    with open(os.path.expanduser(os.path.expandvars(outfile)), 'w') as outfile:
        writer = csv.DictWriter(outfile, output_format['column-sequence'])
        writer.writeheader()
        for date in sorted(time_periods.keys()):
            period_data = time_periods[date]
            for grouping_key in period_data.keys().sorted():
                out_row = {date_column: date, key_column: grouping_key}
                for account_name, account_change in period_data[grouping_key].iteritems():
                    out_row[account_name] = account_change
                writer.writerow(out_row)

if __name__ == "__main__":
    main()
