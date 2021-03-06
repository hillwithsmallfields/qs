#!/usr/bin/python3
# Program to check spreadsheets for date gaps

import argparse
import csv
import csv_sheet
import datetime
import qsutils

weekday_names = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("-g", "--gap",
                        type=int, default=3,
                        help="""The minimum length of gap to report, in days.""")
    parser.add_argument("-w", "--weekend-ok",
                        action='store_true',
                        help="""Ignore weekend gaps.""")
    parser.add_argument("-o", "--output",
                        default="gaps.csv",
                        help="""The output filename.""")
    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    with open(args.output, 'w') as outstream:
        writer = csv.DictWriter(outstream, fieldnames=['File', 'From date', 'From weekday', 'To', 'Period'])
        writer.writeheader()
        for filename in args.input_files:
            dates = sorted(csv_sheet.csv_sheet(config,
                                               input_filename=filename).rows.keys())
            latest = dates[0]
            for date in dates[1:]:
                difference = date - latest
                if difference.days >= args.gap and not (args.weekend_ok
                                                        and difference.days <= 3
                                                        and latest.weekday() == 4):
                    writer.writerow({'File': filename,
                                     'From date': latest.date(),
                                     'From weekday': weekday_names[latest.weekday()],
                                     'To': date.date(),
                                     'Period': difference.days})
                latest = date

if __name__ == "__main__":
    main()
