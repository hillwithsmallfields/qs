#!/usr/bin/python3

# Time-stamp: <2021-04-22 20:13:43 jcgs>

# Program to merge my Quantified Self files.

import argparse
import csv
import datetime
import math
import os
import re
import shutil

import file_types
import qsutils

def weight_tracker_parser(raw):
    if len(raw) == 0:
        return None
    else:
        return { 'Date': iso8601_date_time(raw[1]), 'Kg': raw[0]}

def weight_tracker_complete_row(row):
    if 'Lbs total' not in row or row['Lbs total'] == '':
        if 'Kg' in row and row['Kg'] != '':
            row['Lbs total'] = float(row['Kg']) * 2.20462
        elif 'Stone' in row and row['Stone'] != '' and 'Lbs' in row and row['Lbs'] != '':
            row['Lbs total'] = math.floor(float(row['Stone'])) * 14 + float(row['Lbs'])
        else:
            print("Warning: Could not derive 'Lbs total' for", row.get('Date', "<unknown date>"), "as neither 'Kg' nor 'Stone'+'Lbs' given in", row)
    if 'Kg' not in row or row['Kg'] == '':
        if 'Lbs total' in row and row['Lbs total'] != '':
            row['Kg'] = float(row['Lbs total']) / 2.20462
        else:
            print("Warning: Could not derive 'Kg' for", row.get('Date', "<unknown date>"), "as 'Lbs total' not given or derived in", row)
    if 'Lbs' not in row or row['Lbs'] == '':
        if 'Lbs total' in row and row['Lbs total'] != '':
            row['Lbs'] = int(row['Lbs total']) % 14
    if 'Lbs total' in row and row['Lbs total'] != '':
        row['Stone'] = math.floor(float(row['Lbs total']) / 14)
    if 'Date number' in row and row['Date number'] != '':
        row['Date number'] = int(float(row['Date number']))
    else:
        row['Date number'] = excel_date(row['Date'])
    if 'St total' not in row or row['St total'] == '':
        if 'Lbs total' in row and row['Lbs total'] != '':
            row['St total'] = float(row['Lbs total']) / 14

def iso8601_date_time(timestamp):
    return timestamp.replace('/', '-').replace(' ', 'T')

def iso8601_date_only(timestamp):
    return timestamp.replace('/', '-')[0:10]

def excel_date(date1):          # from http://stackoverflow.com/questions/9574793/how-to-convert-a-python-datetime-datetime-to-excel-serial-date-number
    temp = datetime.datetime(1899, 12, 31)
    parts = [int(x) for x in date1.split('-')]
    delta = datetime.datetime(parts[0], parts[1], parts[2]) - temp
    return float(delta.days) + (float(delta.seconds) / 86400)

file_type_handlers = {
    'weight': {
        'row_parsers': {
            "Weight Tracker|weight": weight_tracker_parser
        },
        'completer': weight_tracker_complete_row,
        'date': iso8601_date_only
    },
    'temperature': {'date': iso8601_date_only},
    'peak_flow': {'date': iso8601_date_only},
    'handelsbanken': {iso8601_date_time},
    'financisto': {iso8601_date_time}
}

def find_row_parser_for(file_type, filename):
    for pattern, parser in file_type_handlers[file_type]['row_parsers'].items():
        if re.search(pattern, filename):
            return parser
    return None

def main():
    by_date = {}
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--debug", action='store_true')
    parser.add_argument("-o", "--output")
    parser.add_argument("-t", "--type")
    parser.add_argument("-v", "--verbose", action='store_true')
    parser.add_argument("mainfile")
    parser.add_argument('incoming', nargs='*')
    args = parser.parse_args()
    if args.output is None:
        old_base, old_ext = os.path.splitext(args.mainfile)
        shutil.copyfile(args.mainfile, old_base + "-old" + old_ext)
        output = args.mainfile
    else:
        output = args.output
    if args.verbose:
        print("Main input is", args.mainfile, "and output is", output)
        print("Incoming files are", args.incoming)
    fieldnames = file_types.first_row(args.mainfile)
    if args.type is None:
        file_type = file_types.deduce_file_type(args.mainfile)
        if args.verbose:
            print("Deduced file type", file_type)
    else:
        file_type = args.type
    handler = file_type_handlers[file_type]
    with open(args.mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            row_date = handler['date'](row['Date'])
            row['Date'] = row_date
            by_date[row_date] = row
            if args.debug:
                print("mainfile raw date", row['Date'], "converted to", row_date)
        csvfile.close()
    for incoming_file in args.incoming:
        row_parser = find_row_parser_for(file_type, incoming_file)
        if row_parser is None:
            print("Skipping", incoming_file, "as I don't have a parser for", file_type)
        else:
            with open(incoming_file) as incoming:
                inreader = csv.DictReader(incoming)
                for raw in inreader:
                    new_row = row_parser(raw)
                    if new_row is not None:
                        if args.debug and args.verbose:
                            print("row from", incoming_file, "is", new_row)
                        new_row_date = handler['date'](new_row['Date'])
                        if args.debug:
                            print("incoming raw date", new_row['Date'], "-->", new_row_date)
                        new_row['Date'] = new_row_date
                        if new_row_date in by_date:
                            if args.debug:
                                print("merging row")
                            by_date[new_row_date].update(new_row)
                        else:
                            if args.debug:
                                print("adding row")
                            by_date[new_row_date] = new_row
    completer = handler['completer']
    sorted_dates = sorted(by_date.keys())
    for date in sorted_dates:
        if args.debug:
            print("completing", date)
        completer(by_date[date])
    for row in by_date.values():
        for colname in row.keys():
            if colname not in fieldnames:
                fieldnames.append(colname)
    with open(output, 'w') as outstream:
        writer = csv.DictWriter(outstream, fieldnames, quoting=csv.QUOTE_NONE)
        writer.writeheader()
        for date in sorted_dates:
            writer.writerow({k: qsutils.tidy_for_output(v) for k,v in by_date[date].items()})

if __name__ == "__main__":
    main()
