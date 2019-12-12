#!/usr/bin/python3
# Program to filter finance spreadsheets and convert them between formats.

# Originally written to add the automatic payments reported in my bank
# statements to my financisto (Android app) accounts, as I haven't
# been in the habit of doing them as they come in.

import argparse
import conversion
import csv
import csv_sheet
import datetime
import os
import qsutils

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

def convert_spreadsheet(input_sheet,
                        do_all,
                        output_sheet,
                        by_period=None, period_string_length=7,
                        verbose=False, message=None):
    """Process the rows of a spreadsheet, adding the results to another spreadsheet."""
    # The 'amount' specification in the format says what to call the
    # output column in which the amount of this transaction is
    # written.  It may either be a string, or a mapping from an
    # account name (such as "MyBank current") to a string.
    if 'amount' not in output_sheet.format['columns']:
        print("An 'amount' label must be specified in the columns of the output format",
              output_sheet.format.format_name)
        return 1

    for row in input_sheet:
        if verbose:
            print("processing transaction row", row)
        out_row = conversion.construct_canonical_row(
            input_sheet, row,
            output_sheet, output_sheet.format.get('column-defaults', {}),
            message)
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
    parser.add_argument("-r", "--reference", # TODO: for outfile_handling?
                        default=None,
                        help="""Name of the reference spreadsheet.""")

    outfile_handling = parser.add_mutually_exclusive_group(required=True)
    outfile_handling.add_argument("-o", "--output",
                                  help="""File to write combined data to.""")
    outfile_handling.add_argument("-u", "--update",
                                  help="""Name of file to read as primary data, and write combined data back to.""")

    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.load_config(args.verbose,
                                 DEFAULT_CONF if not args.no_default_config else None,
                                 *args.config or ())

    if args.update:
        print("Will update", args.update, "from input files", infile_names)
        infile_names = [args.update] + args.input_files
        reference_filename = args.update
        outfile = args.update
    else:
        infile_names = args.input_files
        outfile = args.output
        reference_filename = args.reference
        if args.verbose:
            print("Will write new output file", outfile, "from input files", infile_names)

    output_format_name = (in_sheets[0].format_name
                          if args.update
                          else args.output_format)
    output_format = config['formats'][output_format_name]

    reference_sheet = account.account(transactions=account.account(csv_sheet.csv_sheet(config, input_filename=reference_filename)), accumulate=True)
    in_sheets = [csv_sheet.csv_sheet(config, input_filename=input_file_name)
                 for input_file_name in infile_names]
    out_sheet = csv_sheet.csv_sheet(config, format_name=output_format_name)

    by_years = {}
    by_months = {}
    by_days = {}

    for sheet_number, input_sheet in enumerate(in_sheets):
        convert_spreadsheet(input_sheet,
                            args.all_rows or (sheet_number == 1 and args.update),
                            out_sheet,
                            verbose=args.verbose, message=args.message)
        first_file = False

    out_sheet.write(os.path.expanduser(os.path.expandvars(outfile)))

    return 0

if __name__ == "__main__":
    main()
