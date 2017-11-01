#!/usr/bin/python

# Program to filter finance spreadsheets and convert them between formats.

# Originally written to add the automatic payments reported in my bank
# statements to my financisto (Android app) accounts, as I haven't
# been in the habit of doing them as they come in.

import argparse
import csv
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

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append')
    parser.add_argument("-n", "--no-default-config")
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("-a", "--all-rows",
                        action='store_true',
                        help="""Convert all rows.
                        Otherwise only the rows for which payee name conversions are given will be converted.""")
    parser.add_argument("-O", "--output-format",
                        default='financisto')
    parser.add_argument("-v", "--verbose",
                        action='store_true')

    outfile_handling = parser.add_mutually_exclusive_group(required=True)
    outfile_handling.add_argument("-o", "--output")
    outfile_handling.add_argument("-u", "--update")

    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()

    config_files = ([DEFAULT_CONF]
                    if os.path.exists(DEFAULT_CONF) and not args.no_default_config
                    else [])

    if args.config:
        config_files += args.config

    config = qsutils.load_config(args.verbose, *config_files)

    if args.update:
        infile_names = [args.update] + args.input_files
        outfile = args.update
        print "Will update", args.update, "from input files", infile_names
    else:
        infile_names = args.input_files
        outfile = args.output
        output_format_name = args.output_format
        print "Will write new output file", outfile, "from input files", infile_names, "with provisional format", output_format_name

    output_rows = {}
    first_file = True

    for input_file_name in infile_names:
        print "reading", input_file_name
        with open(os.path.expanduser(os.path.expandvars(input_file_name))) as infile:
            # Scan over the top rows looking for one that matches one
            # of our known headers.  We count how many rows it took,
            # so we can reposition the stream for the
            # dictionary-producing reader.
            header_row_number = 0
            if args.format and (args.format in config['formats']):
                input_format_name = args.format
            else:
                for sample_row in csv.reader(infile):
                    header_row_number += 1
                    input_format_name = qsutils.deduce_format(sample_row, config['formats'])
                    if input_format_name:
                        break
            if first_file:
                first_file = False
                if args.update:
                    output_format_name = input_format_name
                    print "updating, so set output_format_name to", output_format_name
                output_format = config['formats'][output_format_name]
                out_columns = output_format['columns']
                print "output format is", output_format
                if 'amount' not in out_columns:
                    print "An 'amount' label must be specified in the columns of the output format", output_format_name
                    return 1
                outcol_amount = out_columns['amount']
                out_currency_column = out_columns.get('currency', None)
                default_account_name = output_format.get('name', None)
            input_format = config['formats'][input_format_name]
            in_columns = input_format['columns']
            in_payee_column = in_columns['payee']
            in_credits_column = in_columns.get('credits', None)
            in_debits_column = in_columns.get('debits', None)
            in_account_column = in_columns.get('account', None)
            conversions = input_format.get('conversions', {}) # lookup table for payees by name in input file to real name
            print "reading input file", infile
            infile.seek(0)
            for i in range(1, header_row_number):
                dummy = infile.readline()
            for row in csv.DictReader(infile):
                row = {k:v for k,v in row.iteritems() if k != ''}
                if args.verbose:
                    print "processing transaction row", row
                if in_payee_column not in row:
                    print "payee field", in_payee_column, "missing from row", row
                    continue
                payee_name = row[in_payee_column]
                conversion = conversions.get(payee_name, None)
                # except in "all" mode, we're only importing amounts from payees
                # for which we can convert the name-on-statement to the real name
                if args.all_rows or conversion:
                    if in_credits_column:
                        money_in = row[in_credits_column]
                        money_in = 0 if money_in == '' else float(money_in)
                    else:
                        money_in = 0
                    if in_debits_column:
                        money_out = row[in_debits_column]
                        money_out = 0 if money_out == '' else float(money_out)
                    else:
                        money_out = 0
                    row_date = row[in_columns['date']]
                    row_time = "01:00:00" # todo: make these count up a second for each successive import
                    in_account = row[in_account_column] if in_account_column else default_account_name
                    if (not isinstance(outcol_amount, basestring)) and in_account not in outcol_amount:
                        print "unrecognized in_account", in_account
                        print "while using in_account_column", in_account_column
                    this_outcol_amount = outcol_amount if isinstance(outcol_amount, basestring) else outcol_amount.get(in_account, "Unknown")
                    out_row = {out_columns['date']: row_date,
                               this_outcol_amount: money_in - money_out}
                    if out_currency_column:
                        this_out_currency_column = out_currency_column if isinstance(out_currency_column, basestring) else out_currency_column[in_account]
                        out_row[this_out_currency_column] = output_format['currency']
                    if 'original_amount' in out_columns:
                        out_row[out_columns['original_amount']] = money_in - money_out
                    if 'original_currency' in out_columns:
                        out_row[out_columns['original_currency']] = output_format['currency']
                    if in_account and 'account' in out_columns:
                        out_row[out_columns['account']] = in_account
                    if 'time' in out_columns:
                        out_row[out_columns['time']] = row_time
                    for outcol_name in ['category', 'parent', 'payee', 'location', 'project', 'note']:
                        if outcol_name in out_columns:
                            if conversion and outcol_name in conversion:
                                out_row[out_columns[outcol_name]] = conversion[outcol_name]
                            else:
                                if outcol_name in in_columns:
                                    out_row[out_columns[outcol_name]] = row[in_columns[outcol_name]]
                                # todo: some form of pass-through when not filtering by conversions
                    if args.verbose:
                        print "constructed", out_row
                    output_rows[row_date+"T"+row_time] = out_row

    with open(os.path.expanduser(os.path.expandvars(outfile)), 'w') as outfile:
        writer = csv.DictWriter(outfile, output_format['column-sequence'])
        writer.writeheader()
        for timestamp in sorted(output_rows.keys()):
            writer.writerow(output_rows[timestamp])

if __name__ == "__main__":
    main()
