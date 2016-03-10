#!/usr/bin/python

# Time-stamp: <2016-03-10 21:32:13 jcgs>

# Program to merge my Quantified Self files.

import argparse
import csv

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
        with open(incoming_file) as incoming:
            pass
            # todo: get file type by analyzing name, and choose custom reader
            # inreader = csv.DictReader(incoming)
            # for row in inreader:
            #     print "Got incoming row", row

if __name__ == "__main__":
    main()
