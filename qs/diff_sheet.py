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
                 filter_a_col=None, filter_a_val=None, 
                 initial_b=0, track_b=None,
                 filter_b_col=None, filter_b_val=None):
        super().__init__(sheet_a.config)
        self.rows = {}
        print("diff: result_column:", result_column,
              "sheet_a:", sheet_a,
              "column_a:", column_a,
              "initial_a:", initial_a,
              "track_a:", track_a,
              "filter_a_col:", filter_a_col,
              "filter_a_val:", filter_a_val,
              "sheet_b:", sheet_b,
              "column_b:", column_b,
              "initial_b:", initial_b,
              "track_b:", track_b,
              "filter_b_col:", filter_b_col,
              "filter_b_val:", filter_b_val,)
        column_a_out = column_a if column_a != column_b and column_a != track_a and column_a != track_b else column_a + "(a)"
        column_b_out = column_b if column_b != column_a and column_b != track_a and column_b != track_b else column_b + "(b)"
        track_a_out = track_a if track_a != column_a and track_a != column_b and track_a != track_b else track_a + "(A)"
        track_b_out = track_b if track_b != column_a and track_b != column_b and track_b != track_a else track_b + "(B)"
        self.column_a = column_a_out
        self.track_a = track_a_out
        amount_a = 0
        balance_a = initial_a
        rows_a = sheet_a.rows
        self.column_b = column_b_out
        self.track_b = track_b_out
        amount_b = 0
        balance_b = initial_b
        rows_b = sheet_b.rows
        self.result_column = result_column
        self.colseq = ['timestamp', column_a_out]
        if track_a:
            self.colseq.append(track_a_out)
        self.colseq.append(column_b_out)
        if track_b:
            self.colseq.append(track_b_out)
        self.colseq.append(result_column)
        self.colseq.append('change')
        self.colseq.append('category')
        self.colseq.append('payee')
        self.datacols = self.colseq[1:]
        prev_row = {}
        prev_difference = 0
        for ts in sorted(set([k for k in rows_a.keys()] + [k for k in rows_b.keys()])):
            category = None
            payee = None
            if track_a:
                amount_a = 0
            if ts in rows_a:
                row_a = rows_a[ts]
                if not filter_a_col or filter_a_col not in row_a or row_a[filter_a_col] == filter_a_val:
                    category = row_a.get('category')
                    payee = row_a.get('payee')
                    amount_a = float(row_a.get(column_a, 0))
                    if track_a:
                        balance_a += amount_a
            if track_b:
                amount_b = 0
            if ts in rows_b:
                row_b = rows_b[ts]
                if not filter_b_col or filter_b_col not in row_b or row_b[filter_b_col] == filter_b_val:
                    if not category:
                        category = row_b.get('category')
                    if not payee:
                        payee = row_b.get('payee')
                    amount_b = float(row_b.get(column_b, 0))
                    if track_b:
                        balance_b += amount_b
            difference = ((balance_a if track_a else amount_a)
                          - (balance_b if track_b else amount_b))
            row = {'timestamp': ts,
                   result_column: difference,
                   'change': difference - prev_difference,
                   column_a_out: amount_a,
                   column_b_out: amount_b}
            if track_a:
                row[track_a_out] = balance_a
            if track_b:
                row[track_b_out] = balance_b
            if category:
                row['category'] = category
            if payee:
                row['payee'] = payee
            prev_difference = difference
            if not self.same_row_data(row, prev_row):
                self.rows[ts] = row
                prev_row = row

    def same_row_data(self, a, b):
        for k in self.datacols:
            if a.get(k) != b.get(k):
                return False
        return True
        
    def column_names_list(self):
        return self.colseq

    def write_csv(self, filename, suppress_timestamp=False):
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
