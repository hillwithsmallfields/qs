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

POSSIBLE_COLUMN_NAMES = {'stones': 'stone',
                         'pounds': 'pound',
                         'lbs': 'pound',
                         'kilograms': 'kilogram',
                         'kgs': 'kilogram'}

COLUMN_LABELS = {'stone': "Stone",
                 'kilogram': "Kg",
                 'pound': "Pounds",
                 'calories': "Calories",
                 'breakfast': "Breakfast calories",
                 'lunch': "Lunch calories",
                 'dinner': "Dinner calories",
                 'snacks': "Snack calories",
                 # todo: are these calories or weights?
                 'carbohydrates': "Carbohydrates",
                 'fat': "Fat",
                 'protein': "Protein",
                 'sugar': "Sugar"
}

def column_label(column):
    return COLUMN_LABELS.get(column, column)

COLUMN_HEADERS = {'stone': 'St total',
                  'pound': 'Lbs total',
                  'kilogram': 'Kg',
                  'calories': 'calories',
                  'breakfast': 'breakfast_cals',
                  'lunch': 'lunch_cals',
                  'dinner': 'dinner_cals',
                  'snacks': 'snacks_cals',
                  'carbohydrates': "carbohydrates",
                  'fat': "fat",
                  'protein': "protein",
                  'sugar': "sugar"
}

def column_header(column):
    return COLUMN_HEADERS.get(column, column)

def munge_weights(data):
    data['Lbs total'] = pd.to_numeric(data['Stone']) * 14 + data['Lbs']
    data['St total'] = data['Lbs total'] / 14
    data['Kg'] = data['Lbs total'] * 0.453592

def munge_finances(data):
    # data['Date'] = data['timestamp']
    pass

def munge_calories(data):
    pass

MUNGERS = {
    'weight': munge_weights,
    'calories': munge_calories,
    'finances': munge_finances
}

def normalize_column_name(unit_name):
    for k, v in POSSIBLE_COLUMN_NAMES.items():
        if k.startswith(unit_name):
            return v
    return None

def parsetime(timestr):
    return datetime.datetime.strptime(timestr, "%Y-%m-%d") if isinstance(timestr, str) else timestr

def qscharts(mainfile, file_type, columns, begin, end, match, outfile_template, plot_param_sets):
    for name_suffix, params in plot_param_sets.items():
        qschart(mainfile, file_type, columns, begin, end, match, outfile_template % name_suffix, **params)

def qschart(mainfile, file_type, columns, begin, end, match, outfile, **plot_params):

    data = pd.read_csv(mainfile, parse_dates=['Date'])

    if begin:
        data = data.loc[data['Date'] >= begin]
    if end:
        data = data.loc[data['Date'] <= end]
    if match:
        pass                    # TODO: filter data

    data.set_index("Date")

    if file_type in MUNGERS:
        MUNGERS[file_type](data)

    fig, axs = plt.subplots(**plot_params) # https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.figure.html for more parameters

    # TODO: label every year; grid lines?
    # TODO: plot absolute values
    for column in columns:
        data.loc[data[column_header(column)] != 0,
                 ['Date', column_header(column)]].plot(ax=axs, x="Date", y=column_header(column))
        plt.ylabel(column_label(column))

    plt.xlabel("Date")
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

    parser.add_argument("--column",
                        action='append')

    parser.add_argument("mainfile")
    args = parser.parse_args()
    if args.type is None:
        file_type = file_types.deduce_file_type(args.mainfile)
        if args.verbose:
            print("Deduced file type", file_type)
    else:
        file_type = args.type

    qschart(args.mainfile,
            file_type,
            [normalize_column_name(column) for column in args.column],
            args.begin and parsetime(args.begin),
            args.end and parsetime(args.end),
            args.match,
            args.output)

if __name__ == "__main__":
    main()
