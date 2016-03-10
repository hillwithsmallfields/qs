#!/usr/bin/python

# Time-stamp: <2016-03-10 22:03:29 jcgs>

# Program to merge my Quantified Self files.

import argparse
import csv
import re

def weight_tracker_parser(raw):
    return raw

row_parsers = {
    "Weight Tracker": weight_tracker_parser
}

def find_row_parser_for(filename):
    for pattern, parser in row_parsers:
        if re.search(pattern, filename):
            return parser
    return None

def main():
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
            print "Got row", row
        csvfile.close()
    for incoming_file in args.incoming:
        row_parser = find_row_parser_for(incoming_file)
        with open(incoming_file) as incoming:
            inreader = csv.Reader(incoming)
            for raw in inreader:
                row = row_parser(raw)
                print "Got incoming row", row

if __name__ == "__main__":
    main()
