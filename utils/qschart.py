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
                 'systolic': "Systolic",
                 'diastolic': "Diastolic",
                 'heart_rate': "Heart rate",
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
                  'systolic': 'SYS',
                  'diastolic': 'DIA',
                  'heart_rate': 'Pulse',
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

def munge_sleep(data):
    data['Latency'] = data['onset_latency'] / 3600.0
    for field in ['Duration',
                  'Awake',
                  'Light',
                  'Rem',
                  'Restless',
                  'Deep',
                  'Total']:
        data[field] = data[field.lower()] / 3600.0

MUNGERS = {
    'weight': munge_weights,
    'calories': munge_calories,
    'finances': munge_finances,
    'sleep': munge_sleep
}

def normalize_column_name(unit_name):
    for k, v in POSSIBLE_COLUMN_NAMES.items():
        if k.startswith(unit_name):
            return v
    return None

def parsetime(timestr):
    return datetime.datetime.strptime(timestr, "%Y-%m-%d") if isinstance(timestr, str) else timestr

def qscharts(mainfile, file_type,
             columns, begin, end, match, by_day_of_week,
             outfile_template,
             plot_param_sets,
             bar=False):
    # print("charting", mainfile)
    for name_suffix, params in plot_param_sets.items():
        qschart(mainfile, file_type, columns, begin, end, match, by_day_of_week, outfile_template % name_suffix, params, bar=bar)

def plot_column_set(axs, data, columns, prefix, bar=False):
    for column in columns:
        column_data = data.loc[data[column_header(column)] != 0,
                               ['Date', column_header(column)]]
        if not column_data.empty:
            if bar:
                # https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.plot.bar.html
                # for how to set the colour
                column_data.plot.bar(ax=axs, x="Date", y=column_header(column))
            else:
                column_data.plot(ax=axs, x="Date", y=column_header(column))
            # TODO: it's not including the prefix (which I'm using for the day of the week)
            plt.ylabel(prefix + column_label(column))

def qschart(mainfile, file_type, columns, begin, end, match, by_day_of_week, outfile, plot_params, bar=False):

    # TODO: rolling averages, as in http://jonathansoma.com/lede/foundations-2018/pandas/rolling-averages-in-pandas/
    # TODO: filter by day of week
    # TODO: check timestamps of mainfile and outfile

    data = pd.read_csv(mainfile, parse_dates=['Date'])

    # do this before trimming to 'begin' and 'end', as this may create
    # the date column they need:
    if file_type in MUNGERS:
        MUNGERS[file_type](data)

    if begin:
        # print("begin is of type", type(begin), "and value", begin)
        data = data.loc[data['Date'] >= begin]
    if end:
        data = data.loc[data['Date'] <= end]
    if match:
        pass                    # TODO: filter data

    data.set_index("Date")

    fig, axs = plt.subplots(**plot_params) # the background colour comes in here
    # https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.figure.html for more parameters

    axs.set_facecolor(fig.get_facecolor())

    # TODO: label every year; grid lines?
    # TODO: plot absolute values

    plot_column_set(axs, data, columns,
                    "All " if by_day_of_week else "",
                    bar=bar)

    if by_day_of_week:
        for dow in range(7):
            plot_column_set(axs,
                            data[data['Date'].dt.dayofweek == dow],
                            columns,
                            "Day_%d_" % dow,
                            bar=bar)

    plt.xlabel("Date")
    plt.grid(axis='both')

    fig.savefig(outfile,
                facecolor=fig.get_facecolor())

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
