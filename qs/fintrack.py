#!/usr/bin/python

# Program to track finance spreadsheets

import argparse
import csv
import os
import qsutils

# See notes in finconv.py for config file format

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append')
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
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

    parser.add_argument("input_file", nargs="?")
    args = parser.parse_args()

    config_files = ([DEFAULT_CONF]
                    if os.path.exists(DEFAULT_CONF) and not args.no_default_config
                    else [])

    if args.config:
        config_files += args.config

    config = qsutils.load_config(args.verbose, *config_files)

    if args.update:
        infile_name = args.update
        outfile_name = args.update
        print "Will update", args.update, "from input files", infile_names
    else:
        infile_name = args.input_file
        outfile_name = args.output
        output_format_name = args.output_format
        if args.verbose:
            print "Will write new output file", outfile, "from input files", infile_names, "with provisional format", output_format_name

    output_format = config['formats'][output_format_name]

    # the keys of the "trackers" part of the format are the tracking
    # columns, and the values of it are the transaction columns being
    # tracked
    trackers = output_format['trackers']
    tracking_values = { key: 0 for key in trackers.keys() }

    comparisons = output_format['comparisons']

    # todo: use process_fin_csv_rows

    with open(os.path.expanduser(os.path.expandvars(outfile_name)), 'w') as outfile:
        writer = csv.DictWriter(outfile, output_format['column-sequence'])
        writer.writeheader()
        with open(os.path.expanduser(os.path.expandvars(args.input_file))) as infile:
            for row in csv.DictReader(infile):
                for tracker, tracked in trackers.iteritems():
                    # print "old from row: ", row.get(tracker, "<na>")
                    # print "old from memory:", tracking_values[tracker]
                    # print "change from row: ", row.get(tracked, "<na>")
                    old = row.get(tracker, None)
                    # if there's no value for this cell, use the most recent available
                    if old is None or old == "":
                        old = tracking_values[tracker]
                    new = row.get(tracked, None)
                    if new is None or new == "":
                        new = 0
                    tracking_values[tracker] = round(float(old) + float(new), 2)
                    row[tracker] = tracking_values[tracker]
                for difference_col, pair in comparisons.iteritems():
                    a = row.get(pair[0], None)
                    b = row.get(pair[1], None)
                    if a and b:
                         # sometimes they come in as strings, so use float()
                        row[difference_col] = round(float(a) - float(b), 2)
                writer.writerow(row)

if __name__ == "__main__":
    main()
