#!/usr/bin/python3
# Program to filter finance spreadsheets and convert them between formats.

# Originally written to add the automatic payments reported in my bank
# statements to my financisto (Android app) accounts, as I haven't
# been in the habit of doing them as they come in.

import argparse
import csv
import csv_sheet
import datetime
import os
import qsutils
import re

# The config file should look like this:

# formats:
#   handelsbanken:
#     column-sequence: ["Date", None, "Details", None, "Money out", None, "Money in", None, "Balance"]
#     columns:
#       date: Date
#       payee: Details
#       credits: Money in
#       debits: Money out
#     currency: "GBP"
#     name: "Handelsbanken current account"
#     conversions:
#       "MYEMPLOYER LTD":
#         'category': "Salary"
#         'parent': "Incoming"
#         'payee': "My Employer Ltd"
#       "MY COUNTY COUNCIL":
#         'category': "Government"
#         'parent': "Routine costs"
#         'payee': "Shire Council"
#   financisto:
#     column-sequence: ["date", "time", "account", "amount", "currency", "original amount", "original currency", "category", "parent", "payee", "location", "project", "note"]
#     columns:
#       date: date
#       payee: payee

# todo: add to format 'accounts' which indicates which of the columns are accounts

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def find_conversion(conversions, payee_name):
    """Find a mapping from the input format to the output, for a named payee."""
    for key, value in conversions.items():
        if re.match(key, payee_name):
            return value
    return None

def add_to_period(out_row, by_periods, date_length):
    """Add this row to the transactions for a given period.
    The period is produced by truncating the date string.  The
    parameter by_periods is a dictionary mapping dates to maps of
    payees to lists of transaction rows.  Returns True if there was
    already a transaction in that period for that payee, which can be
    used for avoiding making duplicate entries."""
    month = row_date[:date_length]
    if month not in by_periods:
        by_periods[month] = {}
    month_by_payees = by_periods[month]
    if payee in month_by_payees:
        month_by_payees[payee].append(out_row)
        return True
    else:
        month_by_payees[payee] = [out_row]
        return False

def construct_canonical_row(input_sheet, row, output_sheet, out_column_defaults, message=None):
    input_format = input_sheet.format
    in_columns = input_format['columns']
    in_date_column = in_columns['date']
    in_payee_column = in_columns['payee']
    in_account_column = in_columns.get('account', None)
    if in_date_column not in row:
        print("Date column", in_date_column, "not present in row", row)
        return None
    row_date = qsutils.normalize_date(input_sheet.get_cell(row, 'date', None))
    if row_date is None:
        print("empty date from row", row)
        return None
    # This is the name of the input spreadsheet, to help trace rows
    # that don't have an account name cell:
    default_account_name = input_format.get('name', "Unknown")
    in_time_column = in_columns.get('time', None)
    conversions = input_format.get('conversions', {}) # lookup table for payees by name in input file to name in output file
    payee_name = input_sheet.get_cell(row, 'payee', None)
    if payee_name is None:
        print("payee field missing from row", row)
        return None
    output_format = output_sheet.format
    out_columns = output_format['columns']
    out_column_defaults = output_format.get('column-defaults', {})
    # The 'amount' specification in the format says what to call the
    # output column in which the amount of this transaction is
    # written.  It may either be a string, or a mapping from an
    # account name (such as "MyBank current") to a string.
    if 'amount' not in out_columns:
        print("An 'amount' label must be specified in the columns of the output format", output_format_name)
        return 1
    outcol_for_amount = out_columns['amount']
    out_currency_column = out_columns.get('currency', None)
    row_time = input_sheet.get_cell(row, 'time', out_column_defaults.get('time', "01:02:03"))
    row_timestamp = output_sheet.unused_timestamp_from(row_date, row_time)
    currency = row.get('currency', input_format.get('currency', "?")) # todo: this looks wrong, it shouldn't use a hardwired column name
    money_in = input_sheet.get_numeric_cell(row, 'credits', 0)
    money_out = input_sheet.get_numeric_cell(row, 'debits', 0)

    # The 'account' column in the input, if present, indicates
    # which account this transaction is on.
    #
    # In the output, we may either:
    #
    # * Translate this to a single other column, in which case
    #   the output format will specify its name as the 'amount'
    #
    # * Put it into one of one of several account columns,
    #   according to the 'account' cell of the input sheet.
    #   In this case, the 'amount' of the output format must
    #   be a mapping from input values of the 'account' cell
    #   to column names for the output row.
    #
    # The output sheet can also have an 'account' column.

    in_account = input_sheet.get_cell(row, 'account',
                                      default_account_name)
    if ((not isinstance(outcol_for_amount, str))
        and in_account not in outcol_for_amount):
        print("----------------")
        print("unrecognized in_account", in_account, "in row", row)
        print("recognized values are:")
        for colkey, colval in outcol_for_amount.items():
            print("    ", colkey, colval)
    this_outcol_for_amount = (outcol_for_amount
                              if isinstance(outcol_for_amount, str)
                              else outcol_for_amount.get(in_account,
                                                         default_account_name))
    out_row = {out_columns['date']: row_date,
               this_outcol_for_amount: money_in - money_out}
    if in_account and 'account' in out_columns:
        out_row[out_columns['account']] = in_account
    if out_currency_column:
        this_out_currency_column = (out_currency_column
                                    if isinstance(out_currency_column, str)
                                    else out_currency_column[in_account])
        out_row[this_out_currency_column] = currency
    if 'original_amount' in out_columns:
        out_row[out_columns['original_amount']] = money_in - money_out
    if 'original_currency' in out_columns:
        out_row[out_columns['original_currency']] = output_format['currency']
    if 'time' in out_columns:
        out_row[out_columns['time']] = row_time
    # For this group of columns, there may be some literals in
    # the "conversions" (payee descriptions) in the format
    # description.  This is how payee names are translated
    # from the naming scheme of the input sheet to that of the
    # output sheet.
    conversion = find_conversion(conversions, payee_name)
    for canonical_outcol in ['balance', 'category', 'parent', 'payee', 'location', 'project', 'message']:
        # Does the canonical name map to a column name in the output sheet?
        if canonical_outcol in out_columns:
            # The canonical name maps to a column name in the
            # output sheet; write a literal there from the
            # payee conversion description
            if conversion and canonical_outcol in conversion:
                out_row[out_columns[canonical_outcol]] = conversion[canonical_outcol]
            else:
                if ( # does the canonical name map to an input column name?
                    canonical_outcol in in_columns
                    # does the canonically named column have a default output value?
                    or canonical_outcol in out_column_defaults
                   ):
                    # the canonical column name can either map
                    # to a string naming an output column, or
                    # to a mapping from account name to output
                    # column
                    output_column_naming = out_columns[canonical_outcol]
                    outcol_name = (output_column_naming
                                   if isinstance(output_column_naming, str)
                                   # allow for an input column deciding which output column to use
                                   else output_column_naming.get(in_account))
                    try:
                        # TODO: add comments with example values
                        in_column_selector = (in_columns[canonical_outcol]
                                              if canonical_outcol in in_columns
                                              else None)
                        extra_value = (row[in_column_selector]
                                       if in_column_selector in row
                                       else out_column_defaults[canonical_outcol])
                        # the join is initially for financisto's category parents:
                        out_row[out_columns[outcol_name]] = (':'.join(extra_value)
                                                             if isinstance(extra_value, list)
                                                             else extra_value)
                    except KeyError:
                        print("key", outcol_name, "for", canonical_outcol, "not defined in", out_columns, "with naming scheme", output_column_naming)
    if message and 'message' in out_columns:
        message_column = out_columns['message']
        if message_column not in out_row or not out_row[message_column]:
            out_row[message_column] = message
    return out_row

def convert_spreadsheet(args,
                        input_sheet,
                        do_all,
                        output_sheet,
                        by_period=None, period_string_length=7):
    """Process the rows of a spreadsheet, adding the results to another spreadsheet."""
    # The 'amount' specification in the format says what to call the
    # output column in which the amount of this transaction is
    # written.  It may either be a string, or a mapping from an
    # account name (such as "MyBank current") to a string.
    if 'amount' not in output_sheet.format['columns']:
        print("An 'amount' label must be specified in the columns of the output format", output_sheet.format.format_name)
        return 1

    for row in input_sheet:
        if args.verbose:
            print("processing transaction row", row)
        out_row = construct_canonical_row(input_sheet, row,
                                          output_sheet, output_sheet.format.get('column-defaults', {}),
                                          args.message)
        if out_row is None:
            continue
        if by_period is not None:
            add_to_period(out_row, by_months, period_string_length)
        output_sheet.add_row(out_row)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append',
                        help="""Extra config file (may be given multiple times).""")
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-f", "--format",
                        default=None,
                        help="""Format to use for the output file.""")
    parser.add_argument("-a", "--all-rows",
                        action='store_true',
                        help="""Convert all rows.
                        Otherwise only the rows for which payee name conversions are given will be converted.""")
    parser.add_argument("-O", "--output-format",
                        default='financisto')
    parser.add_argument("-m", "--message",
                        help="""Message to put in the note field of any rows which don't already have something there.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')

    outfile_handling = parser.add_mutually_exclusive_group(required=True)
    outfile_handling.add_argument("-o", "--output",
                                  help="""File to write combined data to.""")
    outfile_handling.add_argument("-u", "--update",
                                  help="""Name of file to read as primary data, and write combined data back to.""")

    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.load_config(args.verbose,
                                 DEFAULT_CONF if not args.no_default_config else None,
                                 *args.config)

    if args.update:
        print("Will update", args.update, "from input files", infile_names)
        infile_names = [args.update] + args.input_files
        outfile = args.update
    else:
        infile_names = args.input_files
        outfile = args.output
        if args.verbose:
            print("Will write new output file", outfile, "from input files", infile_names)

    output_format_name = (in_sheets[0].format_name
                          if args.update
                          else args.output_format)
    output_format = config['formats'][output_format_name]

    in_sheets = [csv_sheet.csv_sheet(config, input_filename=input_file_name)
                 for input_file_name in infile_names]
    out_sheet = csv_sheet.csv_sheet(config, format_name=output_format_name)

    by_years = {}
    by_months = {}
    by_days = {}

    for sheet_number, input_sheet in enumerate(in_sheets):
        convert_spreadsheet(args,
                            input_sheet,
                            args.all_rows or (sheet_number == 1 and args.update),
                            out_sheet)
        first_file = False

    out_sheet.write(os.path.expanduser(os.path.expandvars(outfile)))

    return 0

if __name__ == "__main__":
    main()
