#!/usr/bin/python

# Program to track discrepancies in finance spreadsheets

import argparse
import csv
import os
import qsutils

# See notes in finconv.py for config file format

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def find_discrepancies(args, config, input_format, rows):
    date_element_index = 1 # 0 = year, 1 = month, 2 = day # todo: make this a command-line option
    comparisons = input_format['comparisons'].keys()
    outputs = ['date']
    for comp in comparisons:
        outputs += [ comp + '-accum', comp + '-delta' ]
    output_rows = {}
    comparing_values = {}
    comparing_dates = {}
    date_column_name = input_format['columns']['date']
    for timestamp in sorted(rows.keys()):
        row = rows[timestamp]
        output_row = {}
        for comp in comparisons:
            if comp in row:
                raw = row[comp]
                if raw == '':
                    continue
                row_date = row[date_column_name].split('-')
                significant_date = row_date[date_element_index]
                number = float(raw)
                if comp in comparing_values:
                    if significant_date != comparing_dates[comp]:
                        change = comparing_values[comp] - number
                        comparing_dates[comp] = significant_date
                        comparing_values[comp] = number
                        output_row[comp + '-accum'] = number
                        output_row[comp + '-delta'] = change
                else:
                    comparing_values[comp] = number
                    comparing_dates[comp] = significant_date
        if len(output_row) > 0:
            row_date = row[date_column_name]
            output_row['date'] = row_date
            output_rows[row_date] = output_row
    return outputs, output_rows

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

    qsutils.process_fin_csv(args, config, find_discrepancies)

if __name__ == "__main__":
    main()
