#!/usr/bin/python3

# Time-stamp: <2021-04-23 20:24:33 jcgs>

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

def identity_parser(raw):
    return raw

def weight_tracker_parser(raw):
    if len(raw) == 0:
        return None
    else:
        return { 'Date': iso8601_date_time(raw['Date']), 'Kg': raw['Kg']}

def given(row, name):
    return row.get(name, '') not in (None, '')

def weight_tracker_complete_row(row):
    if not given(row, 'Lbs total'):
        if given(row, 'Kg'):
            row['Lbs total'] = float(row['Kg']) * 2.20462
        elif given(row, 'Stone') and given(row, 'Lbs'):
            row['Lbs total'] = math.floor(float(row['Stone'])) * 14 + float(row['Lbs'])
    if not given(row, 'Kg'):
        if given(row, 'Lbs total'):
            row['Kg'] = float(row['Lbs total']) / 2.20462
    if (not given(row, 'Lbs')) and given(row, 'Lbs total'):
            row['Lbs'] = int(row['Lbs total']) % 14
    if (not given(row, 'Stone')) and given(row, 'Lbs total'):
        row['Stone'] = math.floor(float(row['Lbs total']) / 14)
    if given(row, 'Date number'):
        row['Date number'] = int(float(row['Date number']))
    else:
        row['Date number'] = excel_date(row['Date'])
    if (not given(row, 'St total')) and given(row, 'Lbs total'):
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
            "Weight Tracker": weight_tracker_parser,
            "weight": identity_parser
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

def qsmerge(mainfile, incoming, type_given, output):
    by_date = {}
    fieldnames = file_types.first_row(mainfile)
    file_type = type_given or file_types.deduce_file_type(mainfile)

    handler = file_type_handlers[file_type]
    with open(mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            row_date = handler['date'](row['Date'])
            row['Date'] = row_date
            by_date[row_date] = row
        csvfile.close()
    for incoming_file in incoming:
        row_parser = find_row_parser_for(file_type, incoming_file)
        if row_parser is None:
            print("Skipping", incoming_file, "as I don't have a parser for", file_type)
        else:
            with open(incoming_file) as incoming:
                inreader = csv.DictReader(incoming)
                for raw in inreader:
                    new_row = row_parser(raw)
                    if new_row is not None:
                        new_row_date = handler['date'](new_row['Date'])
                        new_row['Date'] = new_row_date
                        if new_row_date in by_date:
                            by_date[new_row_date].update(new_row)
                        else:
                            by_date[new_row_date] = new_row
    completer = handler['completer']
    sorted_dates = sorted(by_date.keys())
    for date in sorted_dates:
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

def main():
    parser = argparse.ArgumentParser()
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
    qsmerge(args.mainfile, args.incoming, args.type, args.output)

if __name__ == "__main__":
    main()
