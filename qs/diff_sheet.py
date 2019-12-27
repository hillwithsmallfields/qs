#!/usr/bin/python3

import csv_sheet

class diff_sheet(csv_sheet):

    def __init__(self,
                 result_column,
                 sheet_a, column_a,
                 sheet_b, column_b):
        super().__init__(sheet_a.config,
                         verbose=sheet_a.verbose)
        self.rows = {}
        balance_a = 0
        rows_a = sheet_a.rows
        balance_b = 0
        rows_b = sheet_b.rows
        for ts in sorted(rows_a.keys() + rows_b.keys()):
            amount_a = rows_a[ts].get(column_a, 0) if ts in rows_a else 0
            balance_a += amount_a
            amount_b = rows_b[ts].get(column_b, 0) if ts in rows_b else 0
            balance_b += amount_b
            self.rows[ts] = {'timestamp': ts,
                             result_column: balance_a - balance_b,
                             'amount_a': amount_a,
                             'balance_a': balance_a,
                             'amount_b': amount_b,
                             'balance_b': balance_b}

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
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    for filename in args.input_files:
        for all_rows in (False, True):
            print("reading and converting", filename)
            sheet = diff_sheet(config, input_sheet=filename, convert_all=all_rows)
            print("diff sheet from", filename, "is", sheet)
            print("---- begin", len(sheet), "all" if all_rows else "filtered", "diff rows ----")
            for row in sheet:
                print(row)
            print("---- end diff rows ----")

if __name__ == "__main__":
    main()
