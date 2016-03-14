#!/usr/bin/python

# Time-stamp: <2016-03-14 18:08:35 johstu01>

# Program to merge my Quantified Self files.

import argparse
import csv
import re

def weight_tracker_parser(raw):
    return { 'Date': iso8601_date(raw[1]), 'Kg': raw[0]}

row_parsers = {
    "Weight Tracker": weight_tracker_parser
}

def find_row_parser_for(filename):
    for pattern, parser in row_parsers.iteritems():
        if re.search(pattern, filename):
            return parser
    return None

def iso8601_date(timestamp):
    return timestamp.replace('/', '-')[0:10]

def main():
    by_date = {}
    parser = argparse.ArgumentParser()
    parser.add_argument("-o", "--output")
    parser.add_argument("mainfile")
    parser.add_argument('incoming', nargs='+')
    args = parser.parse_args()
    if args.output is None:
        # todo: save a copy of the original
        output = args.mainfile
    else:
        output = args.output
    print "Main input is", args.mainfile, "and output is", output
    print "Incoming files are", args.incoming
    with open(args.mainfile) as csvheaderprobe:
        probereader = csv.reader(csvheaderprobe)
        for row in probereader:
            fieldnames = row
            break
    with open(args.mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            row_date = iso8601_date(row['Date'])
            by_date[row_date] = row
        csvfile.close()
    for incoming_file in args.incoming:
        row_parser = find_row_parser_for(incoming_file)
        if row_parser is None:
            print "Skipping", incoming_file, "as I don't have a parser for it"
        else:
            with open(incoming_file) as incoming:
                inreader = csv.reader(incoming)
                for raw in inreader:
                    new_row = row_parser(raw)
                    new_row_date = new_row['Date']
                    if new_row_date in by_date:
                        existing_row = by_date[new_row_date]
                        for key, value in new_row.iteritems():
                            existing_row[key] = value
                    else:
                        by_date[new_row_date] = new_row
    with open(output, 'w') as outstream:
        writer = csv.DictWriter(outstream, fieldnames)
        writer.writeheader()
        for date in by_date.keys().sorted():
            writer.writerow(by_date[date])

if __name__ == "__main__":
    main()
