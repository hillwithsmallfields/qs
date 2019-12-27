#!/usr/bin/python3

import csv_sheet
import canonical_sheet

class diff_sheet(csv_sheet):

    """A sheet representing the timeline of differences between columns in
    two input sheets (or they could be the same sheet)."""

    def __init__(self,
                 result_column,
                 sheet_a, column_a, track_a,
                 sheet_b, column_b, track_b):
        super().__init__(sheet_a.config,
                         verbose=sheet_a.verbose)
        self.rows = {}
        balance_a = 0
        rows_a = sheet_a.rows
        balance_b = 0
        rows_b = sheet_b.rows
        for ts in sorted(rows_a.keys() + rows_b.keys()):
            amount_a = rows_a[ts].get(column_a, 0) if ts in rows_a else 0
            if track_a:
                balance_a += amount_a
            amount_b = rows_b[ts].get(column_b, 0) if ts in rows_b else 0
            if track_b:
                balance_b += amount_b
            row = {'timestamp': ts,
                   result_column: ((balance_a
                                    if track_a
                                    else amount_a)
                                   -
                                   (balance_b
                                    if track_b
                                    else amount_b)),
                   'amount_a': amount_a,
                   'amount_b': amount_b}
            if track_a:
                row['balance_a'] = balance_a
            if track_b:
                row['balance_b'] = balance_b
            self.rows[ts] = row

    def write_csv(self, filename):

    """Write a differences spreadsheet to a file."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            colseq = ['timestamp', result_column,
                      'amount_a', 'balance_a',
                      'amount_b', 'balance_b']
            writer = csv.DictWriter(outfile, colseq)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                # round the unfortunately-represented floats
                writer.writerow({sk: qsutils.trim_if_float(row.get(sk, None))
                                 for sk in colseq})

# tests

import argparse

def main():
    """Tests for this module."""
    parser = qsutils.program_argparser()
    parser.add_argument("--output", "-o",
                        help="""The file to write the result to.""")
    parser.add_argument("out-column", "-c",
                        default="difference",
                        help="""The column name to use for the differences.""")
    parser.add_argument("column-a",
                        default='balance',
                        help="""The column to use for the first input.""")
    parser.add_argument("--track-a", action='store_true',
                        help="""Use a running total of the values in the first input, instead of the values themselves.""")
    parser.add_argument("column-b",
                        default='balance',
                        help="""The column to use for the second input.""")
    parser.add_argument("--track-b", action='store_true',
                        help="""Use a running total of the values in the second input, instead of the values themselves.""")
    parser.add_argument('file_a')
    parser.add_argument('file_b')
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    result = diff_sheet.diff_sheet(
        args.out_column,
        canonical_sheet.canonical_sheet(args.file_a,
                                        convert_all=True),
        args.column_a,
        args.track_a,
        canonical_sheet.canonical_sheet(args.file_b,
                                        convert_all=True),
        args.column_b,
        args.track_b)
    print("---- begin diff rows ----")
    for row in result:
        print(row)
    print("---- end diff rows ----")
    if args.output:
        result.write_csv(args.output)

if __name__ == "__main__":
    main()
