#!/usr/bin/python

# Time-stamp: <2016-03-13 21:19:25 jcgs>

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

def merge_row(accumulated, incoming):
    so_far = accumulated[incoming[date]]
    # todo: fill this in

def main():
    by_date = {}
    parser = argparse.ArgumentParser()
    parser.add_argument("-o", "--output")
    parser.add_argument("mainfile")
    parser.add_argument('incoming', nargs='+')
    args = parser.parse_args()
    if args.output is None:
        output = args.mainfile
    else:
        output = args.output
    print "Main input is", args.mainfile, "and output is", output
    print "Incoming files are", args.incoming
    with open(args.mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            row_date = iso8601_date(row['Date'])
            by_date[row_date] = row
        csvfile.close()
    for incoming_file in args.incoming:
        row_parser = find_row_parser_for(incoming_file)
        with open(incoming_file) as incoming:
            inreader = csv.reader(incoming)
            for raw in inreader:
                row = row_parser(raw)
                print "Got incoming row", row

if __name__ == "__main__":
    main()
