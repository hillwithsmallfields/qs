#!/usr/bin/python

# Program to track discrepancies in finance spreadsheets

import argparse
import os
import qsutils

# See notes in finconv.py for config file format

def find_discrepancies_row_callback(timestamp, row, output_rows, scratch):
    output_row = {}
    comparing_dates = scratch['comparing_dates']
    comparing_values = scratch['comparing_values']
    for comp in scratch['comparisons']:
        if comp in row:
            raw = row[comp]
            if raw == '':
                continue
            row_date = row[scratch['date_column_name']].split('-')
            significant_date = row_date[scratch['date_element_index']]
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
        row_date = row[scratch['date_column_name']]
        output_row['date'] = row_date
        output_rows[row_date] = output_row

def find_discrepancies_setup(app_data, input_format):
    args = app_data['args']
    """Set up the scratch data for finding discrepancies."""
    outputs = ['date']
    comparisons = input_format['comparisons'].keys()
    for comp in comparisons:
        outputs += [ comp + '-accum', comp + '-delta' ]
    return outputs, {
        'date_element_index':  0 if args.yearly else 1,
        'comparisons': comparisons,
        'comparing_values':{},
        'comparing_dates': {},
        'date_column_name': input_format['columns']['date']}

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("-o", "--output")
    parser.add_argument("-y", "--yearly", "--annual",
                        action='store_true',
                        help="""Show the discrepancies per year instead of per month.""")
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("input_file")

    args = parser.parse_args()

    qsutils.process_fin_csv({'args': args,
                             'config': qsutils.program_load_config(args)},
                            find_discrepancies_setup,
                            find_discrepancies_row_callback,
                            None)

if __name__ == "__main__":
    main()
