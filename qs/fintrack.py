#!/usr/bin/python

# Program to track finance spreadsheets

# Originally written to help to add the automatic payments reported in
# my bank statements to my financisto (Android app) accounts, as I
# haven't been in the habit of doing them as they come in.

import argparse
import csv
import os
import yaml

# See notes in finconv.py for config file format

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        default="~/.qs-conf.yaml")
    parser.add_argument("-f", "--format",
                        default='handelsbanken')
    parser.add_argument("-o", "--output")
    parser.add_argument("-O", "--output-format",
                        default='financisto')
    parser.add_argument("input_file")
    args = parser.parse_args()

    with open(os.path.expanduser(os.path.expandvars(args.config))) as config_file:
        config = yaml.safe_load(config_file)

    print "config is", config

    input_format = config['formats'][args.format]
    in_date = input_format['columns']['date']
    in_payee = input_format['columns']['payee']
    in_credits = input_format['columns']['credits']
    in_debits = input_format['columns']['debits']
    conversions = input_format['conversions']

    output_format = config['formats'][args.output_format]
    out_date = output_format['columns']['date']
    out_payee = output_format['columns']['payee']

    print "input format is", input_format
    print "output format is", output_format

    with open(os.path.expanduser(os.path.expandvars(args.output)), 'w') as outfile:
        writer = csv.DictWriter(outfile, output_format['column-sequence'])
        writer.writeheader()
        with open(os.path.expanduser(os.path.expandvars(args.input_file))) as infile:
            for row in csv.DictReader(infile):
                conversion = conversions.get(row[in_payee], None)
                if conversion:
                    money_in = row[in_credits]
                    money_in = 0 if money_in == '' else float(money_in)
                    money_out = row[in_debits]
                    money_out = 0 if money_out == '' else float(money_out)
                    flow = money_in - money_out
                    tracking_balance = tracking_balance + flow

                    writer.writerow({out_date: row[in_date],
                                     'amount': flow,
                                     'given balance': given_balance,
                                     'tracking balance': tracking_balance})

if __name__ == "__main__":
    main()
