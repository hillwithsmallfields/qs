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
import formatted_sheet
import os
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
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append',
                        help="""Extra config file (may be given multiple times).""")
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')

    parser.add_argument("script_files", nargs='*')
    args = parser.parse_args()

    config = qsutils.load_config(args.verbose,
                                 None,
                                 qsutils.DEFAULT_CONF if not args.no_default_config else None,
                                 *args.config or ())

    script = {}

    for script_file in args.script_files:
        filename = qsutils.resolve_filename(script_file)
        if os.path.exists(filename):
            with open(filename) as script_handle:
                rec_update(script, yaml.safe_load(script_handle))

    actions = script.get('actions', [])

    if 'config' in script:
        config_section = script['config']
        confdir = config_section.get('directory', os.getcwd())
        qsutils.load_config(args.verbose,
                            config,
                            qsutils.DEFAULT_CONF if not args.no_default_config else None,
                            [filename
                             if os.path.isabs(filename)
                             else os.path.join(confdir, filename)
                             for filename in config_section.get('files', ())])

    # output_format_name = (in_sheets[0].format_name
    #                       if args.update
    #                       else args.output_format)
    # output_format = config['formats'][output_format_name]

    base_accounts = {}

    if 'base' in config:
        base_section = config['base']
        base_dir = base_section.get('directory', os.getcwd())
        for base_filename in base_section.get('files', ()):
            if args.verbose:
                print("loading", base_filename, "as base sheet")
            for row in canonical_sheet.canonical_sheet(
                    config,
                    input_sheet=qsutils.resolve_filename(base_filename,
                                                         base_dir),
                    convert_all=True,
                    verbose=args.verbose):
                account_name = row['account']
                if account_name not in base_accounts:
                    base_accounts[account_name] = account.account(account_name)
                base_accounts[account_name].add_row_if_new(row)

    accounts = {}

    if 'incoming' in config:
        incoming_section = config['incoming']
        incoming_dir = incoming_section.get('directory', os.getcwd())
        for incoming_filename in incoming_section.get('files', ()):
            if args.verbose:
                print("loading", incoming_filename, "as incoming sheet")
            for row in canonical_sheet.canonical_sheet(
                    config,
                    input_sheet=qsutils.resolve_filename(incoming_filename,
                                                         incoming_dir),
                    convert_all=True,
                    verbose=args.verbose):
                account_name = row['account']
                if account_name not in accounts:
                    accounts[account_name] = account.account(
                        account_name,
                        base_account=base_accounts.get(account_name, None))
                accounts[account_name].add_row_if_new(row)

    if 'combine_by_day' in actions:
        for ba in base_accounts:
            ba.combine_same_day_entries()
        for a in accounts:
            a.combine_same_day_entries()

    outputs_section = config.get('outputs', {})

    if 'accounts' in outputs_section:
        for acc_name, output_filename in outputs_section['accounts']:
            if acc_name not in accounts:
                print("Account name", acc_name, "not found among", sorted(accounts.keys()))
                continue
            out_data = [[payee,
                         when,
                         ";".join(map(str,payee.by_timestamp[when]))]
                         for when in sorted(payee.by_timestamp.keys())
                        for payee in accounts[acc_name]]
            write_fin_csv(['Payee', 'Date', 'Amounts'],
                          out_data,
                          output_filename)

    # out_sheet = canonical_sheet.canonical_sheet(config)

    # for in_sheet in in_sheets:
    #     for row in in_sheet:
    #         account_name = row['account']
    #         if account_name not in accounts:
    #             accounts[account_name] = account.account(account_name)
    #         accounts[account_name].add_row_if_new(row)

    # for accname, accdata in accounts.items():
    #     print("    Account", accname)
    #     for payee in accdata:
    #         print("        ", payee)
    #         for when in sorted(payee.by_timestamp.keys()):
    #             print("            ", when, " ".join(map(str,payee.by_timestamp[when])))

    # formatted_sheet.formatted_sheet(config,
    #                                 output_format_name,
    #                                 out_sheet).write(os.path.expanduser(os.path.expandvars(outfile)))

    return 0

if __name__ == "__main__":
    main()
