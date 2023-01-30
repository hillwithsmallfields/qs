import csv
import datetime
import os.path
import re
import yaml

import sys
from frozendict import frozendict

MAIN_ACCOUNTING_FILE = "~/Sync/finances/finances.csv"
MAIN_HEADERS = ('date', 'time', 'account', 'amount', 'currency',
                'original_amount', 'original_currency', 'balance', 'statement',
                'payee', 'category', 'project', 'item', 'message', 'combicount')

BANK_BASE = os.path.expanduser("~/Sync/finances/handelsbanken/handelsbanken-base.csv")
BANK_FULL = os.path.expanduser("~/Sync/finances/handelsbanken/handelsbanken-full.csv")
BANK_COLUMNS = ('Date', 'Details', 'Money out', 'Money in', 'Balance')

UPDATES_GLOB = os.path.expanduser("~/Downloads/Transactions*.csv")

CONVERSIONS = os.path.expanduser("~/Sync/finances/conversions.yaml")
CONVERSION_TABLE = os.path.expanduser("~/Sync/finances/conversions.csv")
CATPARENTS = os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/conf/cats.yaml")
BUDGETCATS = os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/conf/budgetting-classes.yaml")

def row_date(row):
    return (row['date']
            if 'date' in row
            else (row['Date']
                  if 'Date' in row
                  else row['Value Date']
                  if '"Value Date"' in row
                  else ""))

def get_either(row, a, b):
    return (row[a]
            if a in row
            else row.get(b))

def row_details(row):
    return without_numeric_tail(get_either(row, 'Details', 'Narrative'))

def row_amount(row):
    return (float(get_either(row, 'Money in', 'Cr Amount') or 0)
            - float(get_either(row, 'Money out', 'Dr Amount') or 0))

def read_transactions(filename,
                      starting=None, ending=None):
    """Returns the contents of a CSV file, as a set of frozendicts.
    If the starting or ending arguments are given, they are used to limit
    the rows returned to those with a 'date' field within those limits."""
    with open(os.path.expanduser(filename)) as instream:
        transactions = sorted(list(csv.DictReader(instream)),
                              key=row_date)
        if starting:
            transactions = onwards(transactions, starting)
        if ending:
            transactions = until(transactions, ending)
        return set(frozendict(row) for row in transactions)

def read_csv(filename):
    """Returns the contents of a CSV file, as a set of frozendicts."""
    with open(os.path.expanduser(filename)) as instream:
        return set(frozendict(row)
                   for row in csv.DictReader(instream))

def write_csv(data, header, filename, sort_key):
    """Writes a CSV file using the given headers and sort key."""
    with open(os.path.expanduser(filename), 'w') as outstream:
        writer = csv.DictWriter(
            outstream,
            fieldnames=(header
                        or list(frozenset().union(*[frozenset(row.keys())
                                                    for row in data]))))
        writer.writeheader()
        for row in sorted(data, key=sort_key):
            writer.writerow(row)

def read_conversions(filename=CONVERSIONS):
    return (read_yaml(filename)['formats']['Default']['conversions']
            if filename.endswith(".yaml")
            else ({row['statement']: row for row in read_csv(filename)}
                  if filename.endswith('.csv')
                  else None))

def datestring(date):
    """For comparing dates read from a CSV file, which will be strings."""
    return (date.isoformat()
            if isinstance(date, datetime.date)
            else (date.isoformat(sep=' ')
                  if isinstance(date, datetime.datetime)
                  else date))

def onwards(table, date):
    """Returns the rows of the table whose dates are at least the given date."""
    date = datestring(date)
    return [row
            for row in table
            if row['date'] >= date]

def until(table, date):
    """Returns the rows of the table whose dates are at most the given date."""
    date = datestring(date)
    return [row
            for row in table
            if row['date'] <= date]

def read_yaml(filename):
    """Returns the contents of a YAML file."""
    with open(os.path.expanduser(filename)) as instream:
        return yaml.safe_load(instream)

def without_numeric_tail(string):
    """Returns a string trimmed of a trailing numeric part and leading spaces and stars.

    This gets the useful part of the annotation strings from my bank statements."""
    matched = re.match("^[^-0-9]+", string)
    return matched.group(0).strip().lstrip(" *") if matched else string

def headings(table):
    """Returns a set of the keys of all rows of a list of dicts."""
    return set().union(*(frozenset(row.keys()) for row in table))

def bring_to_front(seq, frontables):
    """Bring specified elements of a list to the front."""
    return [item for item in seq if item in frontables] + [item for item in seq if item not in frontables]

def with_key_as_column(table, key_column_name):
    """Given a dict of dicts, adds the keys of the outer dict to each dict."""
    return [row | {key_column_name: key}
            for key, row in table.items()]

def show_sample(table, n_samples=12):
    table = sorted(table, key=lambda r: (r.get('Date', r.get('date')), r'[Details'))
    for i in range(0, len(table), len(table)//n_samples):
        print(i, table[i])
