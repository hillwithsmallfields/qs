#!/usr/bin/python

# Program to track discrepancies in finance spreadsheets

import argparse
import csv
import os
import qsutils

# See notes in finconv.py for config file format

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

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
    parser.add_argument("input_file")

    args = parser.parse_args()

    config_files = ([DEFAULT_CONF]
                    if os.path.exists(DEFAULT_CONF) and not args.no_default_config
                    else [])

    if args.config:
        config_files += args.config

    config = qsutils.load_config(args.verbose, *config_files)

    date_element_index = 1 # 0 = year, 1 = month, 2 = day # todo: make this a command-line option

    with open(os.path.expanduser(os.path.expandvars(args.input_file))) as infile:
        if args.format and (args.format in config['formats']):
            input_format_name = args.format
        else:
            input_format_name, header_row_number = qsutils.deduce_stream_format(infile, config, args.verbose)

        input_format = config['formats'][input_format_name]

        date_column_name = input_format['columns']['date']

        comparisons = input_format['comparisons'].keys()
        comparing_values = {}
        comparing_dates = {}

        if args.verbose:
            print "Reading", os.path.expanduser(os.path.expandvars(args.input_file)), "as format", input_format_name, "tracking", comparisons

        header_row_number = 0
        for _ in range(1, header_row_number):
            dummy = infile.readline()
        for row in csv.DictReader(infile):
            row = {k:v for k,v in row.iteritems() if k != ''}
            for comp in comparisons:
                if comp in row:
                    raw = row[comp]
                    if raw == '':
                        continue
                    row_date = row[date_column_name].split('-')
                    significant_date = row_date[date_element_index]
                    print "Got comparison", raw, "for", comp, "on", row_date
                    number = float(raw)
                    if comp in comparing_values:
                        if significant_date != comparing_dates[comp]:
                            print "date", significant_date, "changed since", comparing_dates[comp]
                            print "value changed from", comparing_values[comp], "to", number
                            change = comparing_values[comp] - number
                            print "change is", change
                            comparing_dates[comp] = significant_date
                            comparing_values[comp] = number
                            # todo: construct an output row with 1+2n columns: the date, the total discrepancy that month, and the difference since the previous month
                    else:
                        comparing_values[comp] = number
                        comparing_dates[comp] = significant_date


    # todo: scan input file looking at all the comparison columns, and outputting the last value in each of them for each month, the

if __name__ == "__main__":
    main()
