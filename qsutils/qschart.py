#!/usr/bin/python3

# Program to chart my Quantified Self files.

import argparse
import csv
import datetime
import os.path
import re
# import tempfile

from typing import List

import pandas as pd
import matplotlib.pyplot as plt

from dobishem.nested_messages import BeginAndEndMessages

import qsutils.file_types
import qsutils.qsutils

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
                  'systolic': 'Systolic (a.m.)',
                  'diastolic': 'Diastolic (a.m.)',
                  'heart_rate': 'Resting pulse',
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

def qscharts(data:pd.DataFrame, file_type,
             timestamp,
             columns, begin, end, match, by_day_of_week,
             outfile_template,
             plot_param_sets,
             bar=False,
             vlines=None,
             verbose=False):
    """Plot a set of related charts.

    The charts in the set use the same data but different plot params.

    Sample plot params group:

        {'small': {'figsize': (5,4)},
         'large': {'figsize': (11,8)}}
    """
    with BeginAndEndMessages(f"charting {file_type}", verbose=verbose) as msgs:
        data.set_index("Date")
        for name_suffix, params in plot_param_sets.items():
            msgs.print(f"charting into {outfile_template % name_suffix}")
            if not qschart(data, timestamp, file_type,
                           columns, begin, end, match, by_day_of_week,
                           outfile_template % name_suffix, params,
                           bar=bar, vlines=vlines,
                           messager=msgs,
                           ):
                msgs.print(f"TODO: output instructions for fetching missing data for {file_type}")

def plot_column_set(axs, data, columns, prefix, bar=False, messager=None):
    for column in columns:
        col_header = column_header(column)
        if col_header not in data:
            if messager:
                messager.print("Column %s not present" % col_header)
            else:
                print("Column", col_header, "not present")
            continue
        column_data = data.loc[data[col_header] != 0,
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

def qschart(data: pd.DataFrame,
            timestamp,
            file_type: str, # one of 'weight', 'calories', 'finances', 'sleep'
            columns: List[str],
            begin: datetime.datetime, end: datetime.datetime,
            match,
            by_day_of_week,
            outfile,
            plot_params,
            bar=False,
            vlines=None,
            messager=None):

    """Plot a chart, if it needs updating.

    Returns whether the non-empty chart exists at the end.

    If it returns False, the data probably needs to be fetched.
    """

    # TODO: rolling averages, as in http://jonathansoma.com/lede/foundations-2018/pandas/rolling-averages-in-pandas/
    # TODO: filter by day of week

    if timestamp and os.path.exists(outfile) and os.path.getmtime(outfile) >= timestampe:
        return True

    if begin:
        data = data.loc[data['Date'] >= begin]
    if end:
        data = data.loc[data['Date'] <= end]
    if match:
        pass                    # TODO: filter data

    if data.empty:
        return False

    min_date = data['Date'].min()

    fig, axs = plt.subplots(**plot_params) # the background colour comes in here
    # plot_params is something like {'figsize': (5,4)}
    # https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.figure.html for more parameters

    axs.set_facecolor(fig.get_facecolor())

    if vlines:
    # https://matplotlib.org/stable/gallery/lines_bars_and_markers/vline_hline_demo.html#sphx-glr-gallery-lines-bars-and-markers-vline-hline-demo-py
        axs.vlines([when for when in vlines if when >= min_date],
                   0, 1,
                   transform=axs.get_xaxis_transform(), colors='purple')

    # TODO: label every year; grid lines?
    # TODO: plot absolute values

    plot_column_set(axs, data, columns,
                    "All " if by_day_of_week else "",
                    bar=bar,
                    messager=messager)

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
    return True
