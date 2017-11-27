#!/usr/bin/python

# Time-stamp: <2017-11-27 21:09:29 jcgs>

# Program to chart my Quantified Self files.

import argparse
import csv
import datetime
import qsutils

def handle_stones_row(row):
    return row['Stone']*14 + row['Lbs'] if 'Stone' in row and 'Lbs' in row else None

ROW_HANDLERS = {
    'weight': handle_stones_row,
    'stones': handle_stones_row
}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--debug", action='store_true')
    parser.add_argument("-o", "--output")
    parser.add_argument("-t", "--type")
    parser.add_argument("-v", "--verbose", action='store_true')
    parser.add_argument("mainfile")
    args = parser.parse_args()
    if args.type is None:
        with open(args.mainfile) as csvheaderprobe:
            probereader = csv.reader(csvheaderprobe)
            for row in probereader:
                fieldnames = row
                break               # read only the first row
        file_type = qsutils.deduce_file_type_from_headers(fieldnames)
        if args.verbose:
            print "Deduced file type", file_type
    else:
        file_type = args.type
    if file_type not in ROW_HANDLERS:
        print "No handler for type", file_type
        return
    row_handler = ROW_HANDLERS[file_type]
    results = {}
    with open(args.mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            row['__DATE__'] = datetime.datetime.strptime(row['Date'], "%Y-%m-%d")
            results[row_relative_date] = row_handler(row)
    for date in results.keys().sorted():
        row = results[date]

if __name__ == "__main__":
    main()
