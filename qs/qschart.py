#!/usr/bin/python

# Time-stamp: <2017-12-12 19:19:19 jcgs>

# Program to chart my Quantified Self files.

import argparse
import csv
import datetime
import qsutils
import re

def handle_stones_row(row):
    return row['Stone']*14 + row['Lbs'] if 'Stone' in row and 'Lbs' in row else None

ROW_HANDLERS = {
    'weight': handle_stones_row,
    'stones': handle_stones_row
}

def row_filter(filter_control, row):
    if 'start' in filter_control:
        if row['__DATE__'] < filter_control['start']:
            return False
    if 'end' in filter_control:
        if row['__DATE__'] > filter_control['end']:
            return False
    if 'match' in filter_control:
        if not filter_control['regexp'].match(row['match']):
            return False
    return True

def parsetime(timestr):
    return datetime.datetime.strptime(timestr, "%Y-%m-%d")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--debug", action='store_true')
    parser.add_argument("-o", "--output")
    parser.add_argument("-t", "--type")
    parser.add_argument("-v", "--verbose", action='store_true')

    parser.add_argument("-b", "--begin")
    parser.add_argument("-e", "--end")

    parser.add_argument("-m", "--match", nargs=2)

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

    filter_control = {}
    if args.begin:
        filter_control['begin'] = parsetime(args.begin)
    if args.end:
        filter_control['end'] = parsetime(args.end)
    if args.match:
        filter_control['match'] = args.match[0]
        filter_control['regexp'] = re.compile(args.match[1])

    epoch = datetime.datetime.utcfromtimestamp(0)
    data_rows = {}

    with open(args.mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            rowdate = parsetime(row['Date'])
            row['__DATE__'] = rowdate
            data_rows[rowdate] = row_handler(row)
    results = { data_rows[date]
                for date in data_rows.keys().sorted()
                if row_filter(filter_control, data_rows[date]) }

if __name__ == "__main__":
    main()
