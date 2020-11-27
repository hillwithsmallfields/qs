#!/usr/bin/python3

import canonical_sheet
import qsutils

def tracking_setup(app_data, fmt):
    return [], {'accounts': {},
                'input_column': app_data.get('input_column', 'amount') or 'amount',
                'account_naming_column': app_data.get('account_naming_column', 'account') or 'account',
                'output_column': app_data.get('output_column', 'tracker') or 'tracker'}

def tracking_do_row(row_ts, row_data, output_rows_dict, scratch):
    account = row_data.get(scratch['account_naming_column'], None)
    tracked = scratch['accounts'].get(account, 0) + row_data.get(scratch['input_column'], 0)
    scratch['accounts'][account] = tracked
    row_data[scratch['output_column']] = tracked

def tracking_tidyup(headers, output_rows, scratch):
    return headers, scratch['accounts']

class tracked_sheet(canonical_sheet.canonical_sheet):

    """Like canonical_sheet, but with columns added to do our own tracking of accounts."""

    def __init__(self,
                 config,
                 input_sheet=None,
                 convert_all=False,
                 account_name_template=None,
                 reference_sheet=None,
                 input_column='amount',
                 output_column='tracker',
                 verbose=False):
        super().__init__(config,
                         input_sheet=input_sheet,
                         convert_all=convert_all,
                         account_name_template=account_name_template,
                         reference_sheet=reference_sheet,
                         verbose=verbose)
        app_data = {'input_column': input_column,
                    'output_column': output_column}
        qsutils.process_rows(app_data,
                             None,
                             self.rows,
                             tracking_setup, tracking_do_row, tracking_tidyup)

# tests

import argparse

def main():
    """Tests for this module."""
    parser = qsutils.program_argparser()
    parser.add_argument("input_files", nargs='*')
    parser.add_argument("--tracking-column",
                        default="balance")
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    for filename in args.input_files:
        for all_rows in (False, True):
            print("reading and converting", filename)
            sheet = tracked_sheet(config, input_sheet=filename, convert_all=all_rows, output_column=args.tracking_column)
            print("tracked sheet from", filename, "is", sheet)
            print("---- begin", len(sheet), "all" if all_rows else "filtered", "tracked rows ----")
            for row in sheet:
                print(row)
            print("---- end tracked rows ----")

if __name__ == "__main__":
    main()
