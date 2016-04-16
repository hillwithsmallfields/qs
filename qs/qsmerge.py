#!/usr/bin/python

# Time-stamp: <2016-04-16 21:37:47 jcgs>

# Program to merge my Quantified Self files.

import argparse
import csv
import re

def weight_tracker_parser(raw):
    if len(raw) == 0:
        return None
    else:
        return { 'Date': iso8601_date(raw[1]), 'Kg': raw[0]}

def weight_tracker_complete_row(row):
    if 'Lbs total' not in row or row['Lbs total'] == '':
        if 'Kg' in row and row['Kg'] != '':
            row['Lbs total'] = float(row['Kg']) * 2.20462
        elif 'Stone' in row and row['Stone'] != '' and 'Lbs' in row and row['Lbs'] != '':
            row['Lbs total'] = float(row['Stone']) * 14 + float(row['Lbs'])
        else:
            print "Could not derive 'Lbs total' for row", row
    if 'Kg' not in row or row['Kg'] == '':
        if 'Lbs total' in row and row['Lbs total'] != '':
            row['Kg'] = float(row['Lbs total']) / 2.20462
        else:
            print "Could not derive 'Kg' for row", row
    if 'Lbs' not in row or row['Lbs'] == '':
        if 'Lbs total' in row and row['Lbs total'] != '':
            row['Lbs'] = int(row['Lbs total']) % 14
    if 'Stone' not in row or row['Stone'] == '':
        if 'Lbs total' in row and row['Lbs total'] != '':
            row['Stone'] = int(row['Lbs total']) / 14

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
        return {'Date': iso8601_date(raw[0]) + "T23:59:59",
                'Payee': raw[2],
                'Amount': money_string((-money_value(raw[4])) + money_value(raw[6])),
                'Balance': raw[8],
                'Currency': 'GBP',
                'Account': "Handelsbanken current account",
                'Origin': "statement"
                }

def finances_complete_row(row):
    pass

file_type_handlers = {
    'weight': {
        'row_parsers': {
            "Weight Tracker": weight_tracker_parser
        },
        'completer': weight_tracker_complete_row
    },
    'finances' : {
        'row_parsers': {
            "[0-9]{8}_[0-9]{6}_": financisto_parser,
            "^[0-9]{6}\\.": handelsbanken_parser,
            "handelsbanken": handelsbanken_parser
        },
        'completer': finances_complete_row
    }
}

def find_row_parser_for(file_type, filename):
    for pattern, parser in file_type_handlers[file_type]['row_parsers'].iteritems():
        if re.search(pattern, filename):
            return parser
    return None

def iso8601_date(timestamp):
    return timestamp.replace('/', '-').replace(' ', 'T')

def deduce_file_type_from_headers(headers):
    if 'Kg' in headers:
        return 'weight'
    if 'Currency' in headers:
        return 'finances'
    return 'unknown'

def main():
    by_date = {}
    parser = argparse.ArgumentParser()
    parser.add_argument("-t", "--type")
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
            break               # read only the first row
    if args.type is None:
        file_type = deduce_file_type_from_headers(fieldnames)
    else:
        file_type = args.type
    with open(args.mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            row_date = iso8601_date(row['Date'])
            row['Date'] = row_date
            by_date[row_date] = row
        csvfile.close()
    for incoming_file in args.incoming:
        row_parser = find_row_parser_for(file_type, incoming_file)
        if row_parser is None:
            print "Skipping", incoming_file, "as I don't have a parser for it"
        else:
            with open(incoming_file) as incoming:
                inreader = csv.reader(incoming)
                for raw in inreader:
                    new_row = row_parser(raw)
                    if new_row is not None:
                        new_row_date = new_row['Date']
                        if new_row_date in by_date:
                            existing_row = by_date[new_row_date]
                            for key, value in new_row.iteritems():
                                existing_row[key] = value
                        else:
                            by_date[new_row_date] = new_row
    completer = file_type_handlers[file_type]['completer']
    sorted_dates = sorted(by_date.keys())
    for date in sorted_dates:
        completer(by_date[date])
    with open(output, 'w') as outstream:
        writer = csv.DictWriter(outstream, fieldnames)
        writer.writeheader()
        for date in sorted_dates:
            writer.writerow(by_date[date])

if __name__ == "__main__":
    main()
