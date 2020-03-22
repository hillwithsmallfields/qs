#!/usr/bin/python3

# Time-stamp: <2020-03-22 09:43:32 jcgs>

# Program to merge my Quantified Self files.

import argparse
import csv
import datetime
import math
import os
import qsutils
import re
import shutil

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

def financisto_parser(raw):
    if len(raw) == 0:
        return None
    else:
        # todo: handle transfers between accounts
        return {'Date':              raw[0] + "T" + raw[1],
                'Account':           raw[2],
                'Amount':            raw[3],
                'Currency':          raw[4],
                'Original amount':   raw[5],
                'Original currency': raw[6],
                'Category':          raw[8] + ':' + raw[7],
                'Payee':             raw[9],
                'Location':          raw[10],
                'Note':              raw[12],
                'Origin':            "mobile"
        }

def money_value(s):
    if s == '':
        return 0
    else:
        parts = s.split('.')
        if len(parts) == 1:
            return int(parts[0]) * 100
        else:
            return int(parts[0]) * 100 + int(parts[1])

def money_string(v):
    return "%d.%02d" % (v / 100, abs(v) % 100)

def handelsbanken_parser(raw):
    if len(raw) == 0 or raw[0] == '' or raw[0] == 'Date':
        return None
    else:
        return {'Date': iso8601_date_time(raw[0]) + "T23:59:59",
                'Payee': raw[2],
                'Amount': money_string((-money_value(raw[4])) + money_value(raw[6])),
                'Balance': raw[8],
                'Currency': 'GBP',
                'Account': "Handelsbanken current account",
                'Origin': "statement"
                }

def finances_complete_row(row):
    pass

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
            "Weight Tracker|weight[-_]": weight_tracker_parser
        },
        'completer': weight_tracker_complete_row,
        'date': iso8601_date_only
    },
    'finances' : {
        'row_parsers': {
            "[0-9]{8}_[0-9]{6}_": financisto_parser,
            "^[0-9]{6}\\.": handelsbanken_parser,
            "handelsbanken": handelsbanken_parser
        },
        'completer': finances_complete_row,
        'date': iso8601_date_time
    }
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
    with open(args.mainfile) as csvheaderprobe:
        probereader = csv.reader(csvheaderprobe)
        for row in probereader:
            fieldnames = row
            break               # read only the first row
    if args.type is None:
        file_type = qsutils.deduce_file_type_from_headers(fieldnames)
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
            print("Skipping", incoming_file, "as I don't have a parser for it")
        else:
            with open(incoming_file) as incoming:
                inreader = csv.reader(incoming)
                for raw in inreader:
                    new_row = row_parser(raw)
                    if new_row is not None:
                        new_row_date = handler['date'](new_row['Date'])
                        new_row['Date'] = new_row_date
                        if args.debug:
                            print("incoming raw date", new_row['Date'], "-->", new_row_date)
                        if new_row_date in by_date:
                            if args.debug:
                                print("merging row")
                            existing_row = by_date[new_row_date]
                            for key, value in new_row.items():
                                existing_row[key] = value
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
            writer.writerow({k: qsutils.trim_if_float(v) for k,v in by_date[date].items()})

if __name__ == "__main__":
    main()
