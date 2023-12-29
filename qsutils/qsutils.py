#!/usr/bin/python3
# Common routines for my QS programs

import argparse
import calendar
import csv
import datetime
import functools
import operator
import os
import pprint
import re
import yaml

EXCEL_EPOCH = datetime.date(1899, 12, 31)

def excel_date(date1):          # from http://stackoverflow.com/questions/9574793/how-to-convert-a-python-datetime-datetime-to-excel-serial-date-number
    temp = datetime.datetime(1899, 12, 31)
    parts = [int(x) for x in date1.split('-')]
    delta = datetime.datetime(parts[0], parts[1], parts[2]) - temp
    return float(delta.days) + (float(delta.seconds) / 86400)

def ensure_numeric_dates(table):
    for row in table:
        date = row['Date']
        row['Date number'] = ((datetime.date.fromisoformat(date) if isinstance(date, str) else date) - EXCEL_EPOCH).days
    return table

def tidy_for_output(val):
    """Make a value more neatly printable."""
    return (("%.2F" % val)
            if type(val) is float
            else (None
                  if getattr(val, '_hide_in_csv', False)
                  else (""
                        if (val is None or val == "None")
                        else str(val))))

def within_days(a, b, days):
    return abs(a - b).days <= days

def ensure_directory_for_file(filename):
    """Ensure the directory for a file exists, if possible."""
    dirname = os.path.dirname(filename)
    if dirname != '':
        os.makedirs(dirname, exist_ok=True)

def string_to_bool(string):
    if string in ['yes', 'Yes', 'YES', 'true', 'true', 'TRUE', '1', True, 1, 'Detected']:
        return True
    if string in ['no', 'no', 'NO', 'false', 'false', 'FALSE', '0', '', None, False, 0, 'Not Detected']:
        return False
    print("Value", string, "not understood as boolean, treating as False")
    return False

def sum_amount(iterable, fieldname='amount'):
    """Sum a field in a collection of dictionaries.
    This can be used for a column in a spreadsheet."""
    return functools.reduce(operator.add,
                            [x[fieldname] for x in iterable],
                            0)

def combine_transactions(a, b):
    """Make a transaction representing two given transactions."""
    # first, get all the fields from both
    ab = a.copy()
    ab.update(b)
    if a.get('currency', None) == b.get(currency, None):
        ab['amount'] = a.get('amount', 0) + b.get('amount', 0)
        if 'original_amount' in a or 'original_amount' in b:
            ab['original_amount'] = a.get('original_amount', 0) + b.get('original_amount', 0)
    for k in ('payee', 'account', 'currency', 'category', 'parent', 'location' 'project', 'message'):
        av = a.get(k, None)
        bv = b.get(k, None)
        if av is not None and bv is not None and av != bv:
            ab[k] = a.get(k, "") + ";" + b.get
    return ab

def merge_by_date(by_timestamp, period):
    """Return a dictionary with the entries in the input combined by date.

    `period' is a function which should return the starting
    datetime.datetime of the period containing the date it is
    given."""
    result = {}
    for k, v in by_timestamp.items():
        kpart = period(k)
        result[kpart] = combine_transactions(result[kpart], v) if kpart in result else v
    return result

def earliest_unfetched(data):
    return forward_from(max(data.keys()), 0, 0, 1)

def table_support_css(details_background_colour):
    with open(os.path.join(os.path.dirname(os.path.realpath(__file__)), "hover-details.css")) as css_stream:
        return css_stream.read() % details_background_colour

def write_table_support_css(stream, details_background_colour):
    stream.write(table_support_css(details_background_colour))

def last_update_at_least_about_a_day_ago(filename):
    return ((not os.path.isfile(filename))
            or ((datetime.datetime.fromtimestamp(os.path.getmtime(filename))
                 + datetime.timedelta(hours=23, minutes=30))
                < datetime.datetime.now()))
