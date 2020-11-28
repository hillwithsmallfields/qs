#!/usr/bin/python3

import csv
import os

# import csv_sheet
import base_sheet
import canonical_sheet
import qsutils

class diff_sheet(base_sheet.base_sheet):

    """A sheet representing the timeline of differences between columns in
    two input sheets (or they could be the same sheet)."""

    def __init__(self,
                 result_column,
                 sheet_a, column_a,
                 sheet_b, column_b,
                 initial_a=0, track_a=None,
                 initial_b=0, track_b=None):
        super().__init__(sheet_a.config)
        self.rows = {}
        self.column_a = column_a
        self.track_a = track_a
        amount_a = 0
        balance_a = initial_a
        rows_a = sheet_a.rows
        self.column_b = column_b
        self.track_b = track_b
        amount_b = 0
        balance_b = initial_b
        rows_b = sheet_b.rows
        self.result_column = result_column
        self.colseq = ['timestamp', column_a]
        if track_a:
            self.colseq.append(track_a)
        self.colseq.append(column_b)
        if track_b:
            self.colseq.append(track_b)
        self.colseq.append(result_column)
        for ts in sorted(set([k for k in rows_a.keys()] + [k for k in rows_b.keys()])):
            if ts in rows_a:
                amount_a = float(rows_a[ts].get(column_a, 0))
                if track_a:
                    balance_a += amount_a
            if ts in rows_b:
                amount_b = float(rows_b[ts].get(column_b, 0))
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
                   column_a: amount_a,
                   column_b: amount_b}
            if track_a:
                row[track_a] = balance_a
            if track_b:
                row[track_b] = balance_b
            self.rows[ts] = row

    def column_names_list(self):
        return self.colseq

    def write_csv(self, filename):
        """Write a differences spreadsheet to a file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            writer = csv.DictWriter(outfile, self.colseq)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                # round the unfortunately-represented floats
                writer.writerow({sk: qsutils.tidy_for_output(row.get(sk, ""))
                                 for sk in self.colseq})

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
