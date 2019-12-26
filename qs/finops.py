#!/usr/bin/python3
# Program to do things with finance spreadsheets

# Originally written to add the automatic payments reported in my bank
# statements to my financisto (Android app) accounts, as I haven't
# been in the habit of doing them as they come in.

import account
import argparse
import csv
import canonical_sheet
import datetime
import finfuns
import formatted_sheet
import os
import pprint
import qsutils

# This program is driven by scripts written in yaml format.

# The scripts may contain sections like the following:

# config:
#   directory: ~/my-accounting/configs
#   files:
#     - general-config.yaml
#     - merger-config.yaml
# base:
#    directory: ~/my-accounting/base-files
#    files:
#    - financisto.csv
# incoming:
#   directory: ~/my-accounting/newly-saved-statements
#   files:
#      - handelsbanken.csv
#      - co-op.csv

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
    parser = qsutils.program_argparser()
    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.program_load_config(args)
    script = {}
    qsutils.load_multiple_yaml(script, os.getcwd(), args.script_files)

    print("script is", pprint.pformat(script))

    actions = script.get('actions', [])

    if 'config' in script:
        config_section = script['config']
        confdir = config_section.get('directory', os.getcwd())
        qsutils.load_config(args.verbose,
                            config,
                            confdir,
                            *config_section.get('files', ()))

    # output_format_name = (in_sheets[0].format_name
    #                       if args.update
    #                       else args.output_format)
    # output_format = config['formats'][output_format_name]

    variables = {}

    if 'base' in script:
        base_section = script['base']
        base_dir = base_section.get('directory', os.getcwd())
        for base_filename, account_name in base_section.get('files', {}).items():
            if account_name == "":
                account_name = None
            if args.verbose:
                print("loading", base_filename, "as base sheet for account", account_name)
            for row in canonical_sheet.canonical_sheet(
                    config,
                    input_sheet=qsutils.resolve_filename(base_filename,
                                                         base_dir),
                    convert_all=True,
                    verbose=args.verbose):
                account_name = row['account']
                if account_name not in variables:
                    variables[account_name] = account.account(account_name)
                variables[account_name].add_row_if_new(row)

    if 'incoming' in script:
        incoming_section = script['incoming']
        incoming_dir = incoming_section.get('directory', os.getcwd())
        for incoming_filename, account_name in incoming_section.get('files', {}).items():
            if account_name == "":
                account_name = None
            if args.verbose:
                print("loading", incoming_filename, "as incoming sheet for account", account_name)
            for row in canonical_sheet.canonical_sheet(
                    config,
                    input_sheet=qsutils.resolve_filename(incoming_filename,
                                                         incoming_dir),
                    convert_all=True,
                    account_name_template=account_name,
                    verbose=args.verbose):
                account_name = row['account']
                if account_name not in variables:
                    variables[account_name] = account.account(
                        account_name,
                        base_account=base_accounts.get(account_name, None))
                variables[account_name].add_row_if_new(row)

    for command in script.get('commands', []):
        print("Executing command", command)
        command = finfuns.convert_to_Python(command)
        print("Converted command to", command)
        exec(command)

    return 0

if __name__ == "__main__":
    main()
