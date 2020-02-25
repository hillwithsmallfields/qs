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
    parser.add_argument("--confirm-script", action='store_true')
    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.program_load_config(args)
    script = {}
    qsutils.load_multiple_yaml(script, os.getcwd(), args.script_files)

    if args.confirm_script:
        print("script is", pprint.pformat(script))

    actions = script.get('actions', [])

    if 'config' in script:
        config_section = script['config']
        confdir = config_section.get('directory', os.getcwd())
        config = qsutils.load_config(args.verbose,
                                     config,
                                     confdir,
                                     *config_section.get('files', ()))

    # output_format_name = (in_sheets[0].format_name
    #                       if args.update
    #                       else args.output_format)
    # output_format = config['formats'][output_format_name]

    # Making variables of these names allows us to hack everything in
    # a symbol-like position to be a variable reference:
    variables = {'True': True, 'False': False, 'None': None}

    if 'inputs' in script:
        input_section = script['inputs']
        input_dir = input_section.get('directory', os.getcwd())
        for input_filename, account_name_template in input_section.get('files', {}).items():
            if account_name_template == "":
                account_name_template = "%s"
            input_filename = qsutils.resolve_filename(input_filename, input_dir)
            if args.verbose:
                print("loading", input_filename,
                      "as input sheet for accounts templated with",
                      account_name_template)
            filename_as_list = [input_filename]
            sheet_as_read = canonical_sheet.canonical_sheet(
                config,
                input_sheet=input_filename,
                convert_all=True,
                account_name_template=account_name_template,
                origin_files=filename_as_list,
                verbose=args.verbose)
            variables[os.path.basename(input_filename)] = sheet_as_read
            for row in sheet_as_read:
                account_name = row['account']
                if account_name not in variables:
                    print("making new account", account_name)
                    variables[account_name] = account.account(account_name,
                                                              config=config,
                                                              origin_files=filename_as_list)
                added, why_not = variables[account_name].add_row_if_new(row)
                if not added:
                    print("duplicate found in", input_filename)
                    # print("  incoming", qsutils.dict_to_string_sorted_by_key(why_not[0]))
                    # print("  existing", qsutils.dict_to_string_sorted_by_key(why_not[1]))
                    print("  incoming", qsutils.row_as_string_main_keys(why_not[0]))
                    print("  existing", qsutils.row_as_string_main_keys(why_not[1]))
    # print("After reading inputs, accounts are:")
    # for k, v in variables.items():
    #     if isinstance(v, account.account):
    #         print("  ", v)
    #         for pn, pv in v.payees.items():
    #             print("    ", pn, pv, len(pv.by_timestamp))

    for command in script.get('commands', []):
        if args.confirm_script:
            print("---")
            print("Executing command", command)
        command = finfuns.convert_to_Python(command)
        if args.confirm_script:
            print("Converted command to", command)
        try:
            exec(command)
        except Exception as err:
            print("Error", err, "while running command", command)
            raise err

    return 0

if __name__ == "__main__":
    main()
