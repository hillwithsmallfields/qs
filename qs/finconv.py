#!/usr/bin/python3
# Program to filter finance spreadsheets and convert them between formats.

# Originally written to add the automatic payments reported in my bank
# statements to my financisto (Android app) accounts, as I haven't
# been in the habit of doing them as they come in.

import account
import argparse
import csv
import canonical_sheet
import datetime
import formatted_sheet
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
    parser.add_argument("-r", "--base", # TODO: for outfile_handling?
                        default=None,
                        help="""Name of the base spreadsheet.""")

    outfile_handling = parser.add_mutually_exclusive_group(required=True)
    outfile_handling.add_argument("-o", "--output",
                                  help="""File to write combined data to.""")
    outfile_handling.add_argument("-u", "--update",
                                  help="""Name of file to read as primary data, and write combined data back to.""")

    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.load_config(args.verbose,
                                 None,
                                 qsutils.DEFAULT_CONF if not args.no_default_config else None,
                                 *args.config or ())

    if args.update:
        print("Will update", args.update, "from input files", infile_names)
        infile_names = [args.update] + args.input_files
        base_filename = args.update
        outfile = args.update
    else:
        infile_names = args.input_files
        outfile = args.output
        base_filename = args.base
        if args.verbose:
            print("Will write new output file", outfile, "from input files", infile_names, "using", base_filename, "as base")

    output_format_name = (in_sheets[0].format_name
                          if args.update
                          else args.output_format)
    output_format = config['formats'][output_format_name]

    base_accounts = {}
    accounts = {}

    if args.verbose:
        print("loading", base_filename, "as base sheet")
    base_sheet = canonical_sheet.canonical_sheet(
        config,
        input_sheet=base_filename,
        convert_all=True,
        verbose=args.verbose)
    for row in base_sheet:
        account_name = row['account']
        if account_name not in accounts:
            accounts[account_name] = account.account(account_name)
        base_accounts[account_name].add_row_if_new(row)

    if args.verbose:
        print("loading", infile_names, "as input sheets")
    in_sheets = [canonical_sheet.canonical_sheet(config,
                                                 input_sheet=input_file_name,
                                                 convert_all=True,
                                                 verbose=args.verbose)
                 for input_file_name in infile_names]

    out_sheet = canonical_sheet.canonical_sheet(config)

    for in_sheet in in_sheets:
        for row in in_sheet:
            account_name = row['account']
            if account_name not in accounts:
                accounts[account_name] = account.account(account_name)
            accounts[account_name].add_row_if_new(row)

    for accname, accdata in accounts.items():
        print("    Account", accname)
        for payee in accdata:
            print("        ", payee)
            for when in sorted(payee.by_timestamp.keys()):
                print("            ", when, " ".join(map(str,payee.by_timestamp[when])))

    formatted_sheet.formatted_sheet(config,
                                    output_format_name,
                                    out_sheet).write(os.path.expanduser(os.path.expandvars(outfile)))

    return 0

if __name__ == "__main__":
    main()
