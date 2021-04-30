#!/usr/bin/python3

# Program to chart my Quantified Self files.

import argparse
import csv
import datetime
import re
# import tempfile

import pandas as pd
import matplotlib.pyplot as plt

import file_types
import qsutils

def handle_stone_row(row):
    return (((row['Stone']*14 + row['Lbs']) / 14)
            if 'Stone' in row and 'Lbs' in row
            else (row['Lbs total']/14
                  if 'Lbs total' in row
                  else (row['Kg'] / 6.35029
                        if 'Kg' in row
                          else None)))

def handle_kilogram_row(row):
    return (row['Kg']
            if 'Kg' in row
            else (row['Stone'] * 6.35029
                  if 'Stone' in row
                  else None))

def handle_pound_row(row):
    return (row['Lbs total']
            if 'Lbs total' in row
            else ((row['Stone']*14 + row['Lbs'])
                  if 'Stone' in row and 'Lbs' in row
                  else (row['Kg'] * 2.20462
                        if 'Kg' in row
                        else None)))

ROW_HANDLERS = {
    'stone': handle_stone_row,
    'kilogram': handle_kilogram_row,
    'pound': handle_pound_row,
}

POSSIBLE_UNIT_NAMES = {'stones': 'stone',
                       'pounds': 'pound',
                       'lbs': 'pound',
                       'kilograms': 'kilogram',
                       'kgs': 'kilogram'}

UNIT_LABELS = {'stone': "Stone",
               'kilogram': "Kg",
               'pound': "Pounds"}

UNIT_COLUMNS = {'stone': 'St total',
                'pound': 'Lbs total',
                'kilogram': 'Kg'}

def normalize_unit_name(unit_name):
    for k, v in POSSIBLE_UNIT_NAMES.items():
        if k.startswith(unit_name):
            return v
    return None

def row_filter(filter_control, row):
    if 'begin' in filter_control:
        if row['__DATE__'] < filter_control['begin']:
            return False
    if 'end' in filter_control:
        if row['__DATE__'] > filter_control['end']:
            return False
    if 'match' in filter_control:
        if not filter_control['regexp'].match(row[filter_control['match']]):
            return False
    if filter_control['units'] not in row:
        return False
    if row[filter_control['units']] in (None, "", 0, "0", "0.0"):
        return False
    return True

def parsetime(timestr):
    return datetime.datetime.strptime(timestr, "%Y-%m-%d")

def qschart(mainfile, units, filter_control, outfile):

    data = pd.read_csv(mainfile, parse_dates=['Date'])
    data.set_index("Date")

    data['Lbs total'] = pd.to_numeric(data['Stone']) * 14 + data['Lbs']
    data['St total'] = data['Lbs total'] / 14
    data['Kg'] = data['Lbs total'] * 0.453592

    fig, axs = plt.subplots(figsize=(11,8)) # https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.figure.html for more parameters

    # TODO: label every year; grid lines?
    data.loc[data[UNIT_COLUMNS[units]] > 0, ['Date', UNIT_COLUMNS[units]]].plot(ax=axs, x="Date")

    plt.xlabel("Date")
    plt.ylabel(UNIT_LABELS[units])
    plt.grid(axis='both')

    fig.savefig(outfile)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--debug", action='store_true')
    parser.add_argument("-o", "--output", default="/tmp/weight.png")
    parser.add_argument("-t", "--type")
    parser.add_argument("-v", "--verbose", action='store_true')

    parser.add_argument("-b", "--begin",
                        help="""The earliest date to chart.""")
    parser.add_argument("-e", "--end",
                        help="""The latest date to chart.""")

    parser.add_argument("-m", "--match", nargs=2,
                        help="""The column to match on, and the regexp to match it against.""")

    parser.add_argument("-u", "--units", default="stones")

    parser.add_argument("mainfile")
    args = parser.parse_args()
    if args.type is None:
        file_type = file_types.deduce_file_type(args.mainfile)
        if args.verbose:
            print("Deduced file type", file_type)
    else:
        file_type = args.type
    # if file_type not in ROW_HANDLERS:
    #     print("No handler for type", file_type)
    #     return

    units = normalize_unit_name(args.units)

    row_handler = ROW_HANDLERS[units]

    filter_control = {'units': units}
    if args.begin:
        filter_control['begin'] = parsetime(args.begin)
    if args.end:
        filter_control['end'] = parsetime(args.end)
    if args.match:
        filter_control['match'] = args.match[0]
        filter_control['regexp'] = re.compile(args.match[1])

    qschart(args.mainfile, units, filter_control, args.output)

if __name__ == "__main__":
    main()
