#!/usr/bin/python

# Program to filter finance spreadsheets and convert them between formats.

# Originally written to add the automatic payments reported in my bank
# statements to my financisto (Android app) accounts, as I haven't
# been in the habit of doing them as they come in.

import argparse
import csv
import os
import yaml

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

def deduce_format(first_row, formats):
    condensed_row = [cell for cell in first_row if cell != ""]
    for format_name, format_def in formats.iteritems():
        sequence = [col for col in format_def['column-sequence'] if col]
        if sequence == condensed_row:
            return format_name
        return None

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        default="~/.qs-conf.yaml")
    parser.add_argument("-f", "--format",
                        default='handelsbanken')
    parser.add_argument("-O", "--output-format",
                        default='financisto')

    outfile_handling = parser.add_mutually_exclusive_group(required=True)
    outfile_handling.add_argument("-o", "--output")
    output.add_argument("-u", "--update")

    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()

    with open(os.path.expanduser(os.path.expandvars(args.config))) as config_file:
        config = yaml.safe_load(config_file)

    print "config is", config

    if args.update:
        infile_names = [args.update] + args.input_files
        outfile = args.update
    else:
        infile_names = args.input_files
        outfile = args.output
        output_format_name = args.output_format

    print "input files are", infile_names
    print "outfile is", outfile

    output_rows = {}
    first_file = True

    for input_file_name in infile_names:
        with open(os.path.expanduser(os.path.expandvars(input_file_name))) as infile:
            sample_row = csv.Reader(infile)
            input_format = deduce_format(sample_row, config['formats']) or config['formats'][args.format]
            print "input format for", input_file_name, "is", input_format
            if first_file:
                first_file = False
                if args.update:
                    output_format_name = input_format
                output_format = config['formats'][output_format_name]
                out_date = output_format['columns']['date']
                out_payee = output_format['columns']['payee']
                out_name = output_format['name']
                out_currency = output_format['currency']
                print "output format is", output_format
        in_date = input_format['columns']['date']
        in_payee = input_format['columns']['payee']
        in_credits = input_format['columns']['credits']
        if not isinstance(in_credits, basestring):
            pass                # todo: further lookup for multi-account files
        in_debits = input_format['columns']['debits']
        if not isinstance(in_debits, basestring):
            pass                # todo: further lookup for multi-account files
        conversions = input_format['conversions']
        with open(os.path.expanduser(os.path.expandvars(input_file_name))) as infile:
            for row in csv.DictReader(infile):
                conversion = conversions.get(row[in_payee], None)
                if conversion:
                    money_in = row[in_credits]
                    money_in = 0 if money_in == '' else float(money_in)
                    money_out = row[in_debits]
                    money_out = 0 if money_out == '' else float(money_out)
                    row_date = row[in_date]
                    row_time = "08:00:00"
                    output_rows[row_date+"T"+row_time] = {out_date: row_date,
                                                          'time': row_time,
                                                          'account': out_name, # was "Handelsbanken current account"
                                                          'amount': money_in - money_out,
                                                          'currency': out_currency, # was "GBP"
                                                          'original amount': "",
                                                          'original currency': "",
                                                          'category': conversion['category'],
                                                          'parent': conversion['parent'],
                                                          out_payee: conversion['payee'],
                                                          'location': "",
                                                          'project': "",
                                                          'note': "Imported from bank statement"}

    with open(os.path.expanduser(os.path.expandvars(outfile)), 'w') as outfile:
        writer = csv.DictWriter(outfile, output_format['column-sequence'])
        writer.writeheader()
        for timestamp in sorted(output_rows.keys()):
            writer.writerow(output_rows[timestamp])

if __name__ == "__main__":
    main()
