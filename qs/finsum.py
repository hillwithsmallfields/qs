#!/usr/bin/python

# Program to sum each day's transactions per payee in finance spreadsheets

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
                        default='combined')
    parser.add_argument("-o", "--output")
    parser.add_argument("input_file")
    args = parser.parse_args()

    with open(os.path.expanduser(os.path.expandvars(args.config))) as config_file:
        config = yaml.safe_load(config_file)

    print "config is", config

    days = {}
    
    with open(os.path.expanduser(os.path.expandvars(args.input_file))) as infile:
        input_format = deduce_format(sample_row, config['formats']) or config['formats'][args.format]
        print "input format for", input_file_name, "is", input_format
        in_columns = input_format['columns']
        in_date = in_columns['date']
        in_payee = in_columns['payee']
        account_columns = input_format['accounts']
        for row in csv.DictReader(infile):
            date = row[in_date]
            day_data = days[date] = days.get[date, {in_date: date})
            payee_name = row.get(in_payee, "unknown")
            payee_accounts = day_data[payee_name] = day_data.get(payee_name, {})
            for account_name in account_columns:
                payee_accounts[account_name] = (payee_accounts.get(account_name, 0)
                                                + float(row.get(account_name, "0")))
                        
    with open(os.path.expanduser(os.path.expandvars(outfile)), 'w') as outfile:
        writer = csv.DictWriter(outfile, output_formato['column-sequence'])
        writer.writeheader()
        for date in sorted(days.keys()):
            day_data = days[date]
            for payee_name in day_data.keys().sorted():
                out_row = {in_date: date, in_payee: payee_name}
                for account_name, account_change in day_data[payee_name].iteritems():
                    out_row[account_name] = account_change
                writer.writerow(out_row)
                
if __name__ == "__main__":
    main()
