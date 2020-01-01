#!/usr/bin/python3

import account
import csv
import csv_sheet
import os
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

class canonical_sheet:
    """A financial data-only spreadsheet with a standard set of column names.

    This is the result of reading in a CSV file from a finance
    tracking app or a bank statement, and converting column names and
    translating payee names between naming schemes.
    """

    # not subclassed from csv_sheet, because that has a 'format' and
    # the point of this class is to avoid that.

    canonical_column_sequence = [
        'date',
        'time',
        'timestamp',
        'payee',
        'amount',
        'account',
        'balance',
        'currency',
        'original_amount',
        'original_currency',
        'category',
        'parent',
        'location',
        'project',
        'message'
    ]

    def __init__(self,
                 config,
                 input_sheet=None,
                 convert_all=False,
                 account_name_template=None,
                 reference_sheet=None,
                 origin_files=[],
                 verbose=False):
        self.verbose = verbose
        self.config = config
        if self.verbose:
            print("Making canonical_sheet with input_sheet", input_sheet)
        self.rows = {}
        self.row_order = None
        self.row_cursor = 0
        self.origin_files = origin_files
        if isinstance(input_sheet, str):
            if self.verbose:
                print("Reading", input_sheet, "for conversion")
            input_sheet = csv_sheet.csv_sheet(config, input_filename=input_sheet, verbose=self.verbose)
        if isinstance(input_sheet, csv_sheet.csv_sheet):
            self.config = input_sheet.config
            self.origin_files = input_sheet.origin_files
            print("canonical_sheet.init from csv origin_files now", self.origin_files)
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
            self.origin_files = input_sheet.origin_files
            print("canonical_sheet.init copy origin_files now", self.origin_files)
            # take a copy
            self.rows = {k: {vk: vv for vk, vv in v.items()} for k, v in input_sheet.rows.items()}
        elif isinstance(input_sheet, account.account):
            for payee in input_sheet:
                for timestamp, row in payee:
                    self.rows[timestamp] = row
        elif type(input_sheet) == dict:
            print("made canonical_sheet from row dict")
            self.rows = input_sheet

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
                                         input_format.get('original_currency', "?")),
            'sheet': self}
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
            # otherwise, can we copy it from an input cell?
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
        return {output_column_name: canonical_row.get(canonical_column_name, None)
                for canonical_column_name, output_column_name
                in output_format['columns'].items()}

    def distribute_to_accounts(self, accounts={}, added_row_lists={}):
        """Distribute the rows of the sheet to account values.
        Returns a dictionary of accounts, and a dictionary of the rows
        that were added this time.
        Initial values for the dictionaries may be passed in."""
        for row in self.rows.values():
            account_name = row['account']
            if account_name not in accounts:
                accounts[account_name] = account.account(account_name)
            added_row = accounts[account_name].add_row_if_new(row)
            if added_row:
                if account_name not in added_row_lists:
                    added_row_lists[account_name] = []
                added_row_lists[account_name].append(added_row)
        return accounts, added_row_lists

    def write_csv(self, filename):
        """Write a canonical spreadsheet to a file.
        Any columns not in the canonical format are ignored."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            writer = csv.DictWriter(outfile, canonical_sheet.canonical_column_sequence)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                # select only the columns required for this sheet, and
                # also round the unfortunately-represented floats
                writer.writerow({sk: qsutils.trim_if_float(row.get(sk, None))
                                 for sk in canonical_sheet.canonical_column_sequence})

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
            sheet = canonical_sheet(config, input_sheet=filename, convert_all=all_rows)
            print("canonical sheet from", filename, "is", sheet)
            print("---- begin sample of", len(sheet), "all" if all_rows else "filtered", "canonical rows ----")
            countdown = 16
            for row in sheet:
                print(row)
                countdown -= 1
                if countdown == 0:
                    break;
            print("---- end canonical rows ----")
            accounts, added_row_lists = sheet.distribute_to_accounts()
            print("account names for", "unfiltered" if all_rows else "filtered", filename, "are", sorted(accounts.keys()), "and added_row_lists are", sorted(added_row_lists.keys()))
            for k in sorted(accounts.keys()):
                print(k, accounts[k])
            for k in sorted(added_row_lists.keys()):
                print(k, added_row_lists[k][:8])

if __name__ == "__main__":
    main()
