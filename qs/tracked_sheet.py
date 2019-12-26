#!/usr/bin/python3

import canonical_sheet
import qsutils

def tracking_setup(app_data, fmt):
    return [], {}

def tracking_do_row(row_ts, row_data, output_rows_dict, scratch):
    account = row_data.get('account', None)
    tracked = scratch.get(account, 0) + row_data.get('amount', 0)
    scratch[account] = tracked
    row_data['tracker'] = tracked

def tracking_tidyup(headers, output_rows, scratch):
    return headers, scratch

class tracked_sheet(canonical_sheet.canonical_sheet):

    """Like canonical_sheet, but with columns added to do our own tracking of accounts."""

    def __init__(self,
                 config,
                 input_sheet=None,
                 convert_all=False,
                 account_name_template=None,
                 reference_sheet=None,
                 verbose=False):
        super().__init__(config,
                         input_sheet=input_sheet,
                         convert_all=convert_all,
                         account_name_template=account_name_template,
                         reference_sheet=reference_sheet,
                         verbose=verbose)
        qsutils.process_rows(None,
                             None,
                             self.rows,
                             tracking_setup, tracking_do_row, tracking_tidyup)

# tests

import argparse

def main():
    """Tests for this module."""
    parser = qsutils.program_argparser()
    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    for filename in args.input_files:
        for all_rows in (False, True):
            print("reading and converting", filename)
            sheet = tracked_sheet(config, input_sheet=filename, convert_all=all_rows)
            print("tracked sheet from", filename, "is", sheet)
            print("---- begin", len(sheet), "all" if all_rows else "filtered", "tracked rows ----")
            for row in sheet:
                print(row)
            print("---- end tracked rows ----")

if __name__ == "__main__":
    main()
