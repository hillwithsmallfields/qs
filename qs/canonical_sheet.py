#!/usr/bin/python3

import csv_sheet
import qsutils
import re

# Convert sheets between a canonical format and specified other
# formats.  This may involve renaming columns, and filling in default
# values derived from the payee.

def templated_name(template, name):
    return (name
            if template is None or template == "%s"
            else (template
                  if "%s" not in template
                  else template % name))

def find_conversion(conversions, payee_name):
    """Find a mapping from the input format to the output, for a named payee."""
    for key, value in conversions.items():
        if re.match(key, payee_name):
            return value
    return None

def tracking_setup(app_data, fmt):
    return [], {'source': app_data['source']
                'tracking': app_data['tracking']
                'total': 0.0}

def tracking_do_row(row_ts, row_data, output_rows_dict, scratch):
    scratch['total'] += row_data[scratch['source']]
    row_data[scratch['tracking']] = scratch['total']
    if 'discrepancy_with' in scratch:
        row_data[scratch['discrepancy_to'] = (
            scratch['total'] - row_data[scratch['discrepancy_with']])

def tracking_tidyup(headers, scratch):
    return headers, scratch

class canonical_sheet:
    """A financial data-only spreadsheet with a standard set of column names.

    This is the result of reading in a CSV file from a finance
    tracking app or a bank statement, and converting column names and
    translating payee names between naming schemes.
    """

    def __init__(self,
                 config,
                 input_sheet=None,
                 convert_all=False,
                 account_name_template=None,
                 reference_sheet=None,
                 verbose=False):
        self.verbose = verbose
        if self.verbose:
            print("Making canonical_sheet with input_sheet", input_sheet)
        self.rows = {}
        self.row_order = None
        self.row_cursor = 0
        if isinstance(input_sheet, str):
            if self.verbose:
                print("Reading", input_sheet, "for conversion")
            input_sheet = csv_sheet.csv_sheet(config, input_filename=input_sheet, verbose=self.verbose)
        if isinstance(input_sheet, csv_sheet.csv_sheet):
            if self.verbose:
                print("converting", input_sheet)
            for in_row in input_sheet:
                can_row = self.row_to_canonical(input_sheet, in_row,
                                                reference_sheet=reference_sheet,
                                                account_name_template=account_name_template,
                                                convert_all=convert_all)
                if self.verbose:
                    print("made", can_row, "from", in_row)
                if can_row:
                    if self.verbose:
                        print("storing", can_row)
                    self.rows[can_row['timestamp']] = can_row
        elif isinstance(input_sheet, canonical_sheet):
            # take a copy
            self.rows = {k: {vk: vv for vk, vv in v.items()} for k, v in input_sheet.rows.items()}

    def __iter__(self):
        self.row_order = sorted(self.rows.keys())
        self.row_cursor = -1    # because we pre-increment it
        return self

    def __next__(self):
        self.row_cursor += 1
        if self.row_cursor >= len(self.row_order):
            raise StopIteration
        return self.rows[self.row_order[self.row_cursor]]

    def __len__(self):
        return len(self.rows)

    def __str__(self):
        return ("<canonical spreadsheet with "
                + str(len(self.rows)) + " rows>")

    def row_to_canonical(self,
                         input_sheet, row,
                         convert_all=False,
                         out_column_defaults=None,
                         reference_sheet=None,
                         account_name_template=None,
                         message=None):
        """Convert an input row from its own format to our standard format.
        If convert_all is False, convert only the rows with payees for whom
        the input sheet's format configuration has a conversion entry."""
        if reference_sheet is None:
            reference_sheet = input_sheet
        input_format = input_sheet.format
        in_columns = input_format['columns']
        row_date = qsutils.normalize_date(input_sheet.get_cell(row, 'date', None))
        if row_date is None:
            if self.verbose:
                print("empty date from row", row)
            return None
        payee_name = input_sheet.get_cell(row, 'payee', None)
        if payee_name is None:
            if self.verbose:
                print("payee field missing from row", row)
            return None
        conversion = find_conversion(input_format.get('conversions', {}),
                                     payee_name)
        if conversion is None and not convert_all:
            if self.verbose:
                print("no conversion for row", row)
            return None
        row_time = input_sheet.get_cell(
            row, 'time', ("01:02:03"
                          if out_column_defaults is None
                          else out_column_defaults.get('time', "01:02:03")))
        money_in = input_sheet.get_numeric_cell(row, 'credits', 0)
        money_out = input_sheet.get_numeric_cell(row, 'debits', 0)
        out_row = {
            'date': row_date,
            'time': row_time,
            'timestamp': reference_sheet.unused_timestamp_from(row_date, row_time),
            'amount': money_in - money_out,
            'account': templated_name(account_name_template,
                                      (input_sheet.get_cell(row, 'account', None)
                                       or input_format.get('name', "Unknown"))),
            'currency': row.get('currency',
                                input_format.get('currency', "?")),
            'original_amount': money_in - money_out,
            'original_currency': row.get('original_currency',
                                         input_format.get('original_currency', "?"))}
        if message:
            out_row['message'] = message
        # For this group of columns, there may be some literals in
        # the "conversions" (payee descriptions) in the format
        # description.  This is how payee names are translated
        # from the naming scheme of the input sheet to that of the
        # output sheet.
        for canonical_outcol in ['balance', 'category', 'parent',
                                 'payee', 'location', 'project', 'message']:
            # does the canonically named column have a default output value?
            if conversion and canonical_outcol in conversion:
                out_row[canonical_outcol] = conversion[canonical_outcol]
            # otherwise, can we copy if from an input cell?
            elif canonical_outcol in in_columns:
                out_row[canonical_outcol] = input_sheet.get_cell(row, canonical_outcol)
            # otherwise, are we given this as a default by our caller?
            elif (out_column_defaults is not None
                  and canonical_outcol in out_column_defaults):
                extra_value = out_column_defaults[canonical_outcol]
                # the join is initially for financisto's category parents:
                out_row[canonical_outcol] = (':'.join(extra_value)
                                             if isinstance(extra_value, list)
                                             else extra_value)
        return out_row

    def row_from_canonical(self, output_format, canonical_row):
        """Convert a row from our standard format to a specified one."""
        return {output_column_name: row.get(canonical_column_name, None)
                for canonical_column_name, output_column_name
                in output_format['columns'].items()}

    def add_tracking_column(self, source_column, tracking_column,
                            discrepancy_with_column=None,
                            discrepancy_out_column=None):
        """Return a copy of a sheet with a tracking column added."""
        copy = canonical_sheet(self)
        app_data = {'source': source_column,
                    'tracking': tracking_column}
        if discrepancy_with_column:
            app_data['discrepancy_with'] = discrepancy_with_column
            app_data['discrepancy_to'] = (discrepancy_out_column
                                          or (discrepancy_with_column + "_gap"))
        qsutils.process_rows(app_data,
                             None,
                             copy.rows,
                             tracking_setup, tracking_do_row, tracking_tidyup)
        return copy

# tests

import argparse

def main():
    """Tests for this module."""
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append',
                        help="""Extra config file (may be given multiple times).""")
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')
    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.load_config(args.verbose,
                                 None,
                                 None,
                                 qsutils.DEFAULT_CONF if not args.no_default_config else None,
                                 *args.config)
    for filename in args.input_files:
        for all_rows in (False, True):
            print("reading and converting", filename)
            sheet = canonical_sheet(config, input_sheet=filename, convert_all=all_rows)
            print("canonical sheet from", filename, "is", sheet)
            print("---- begin", len(sheet), "all" if all_rows else "filtered", "canonical rows ----")
            for row in sheet:
                print(row)
            print("---- end canonical rows ----")

if __name__ == "__main__":
    main()
