import csv
import os.path
import re
import yaml
from frozendict import frozendict

MAIN_ACCOUNTING_FILE = "~/Sync/finances/finances.csv"
MAIN_HEADERS = ('date', 'time', 'account', 'amount', 'currency',
                'original_amount', 'original_currency', 'balance', 'statement',
                'payee', 'category', 'project', 'item', 'message', 'combicount')

BANK_BASE = "~/Sync/finances/handelsbanken/handelsbanken-base.csv"
BANK_FULL = "~/Sync/finances/handelsbanken/handelsbanken-full.csv"
BANK_COLUMNS = ('Date', 'Details', 'Money out', 'Money in', 'Balance')

UPDATES_GLOB = "~/Downloads/Transactions*.csv"

CONVERSIONS = "~/Sync/finances/conversions.yaml"
CATPARENTS = "~/open-projects/github.com/hillwithsmallfields/qs/conf/cats.yaml"

def read_csv(filename,
             starting=None, ending=None):
    """Returns the contents of a CSV file, as a list of dictionaries.
    If the starting or ending arguments are given, they are used to limit
    the rows returned to those with a 'date' field within those limits."""
    with open(os.path.expanduser(filename)) as instream:
        transactions = list(csv.DictReader(instream))
        if starting:
            transactions = onwards(transactions, starting)
        if ending:
            transactions = until(transactions, ending)
        return set(frozendict(row) for row in transactions)

def write_csv(data, header, filename, sort_key):
    """Writes a CSV file using the given headers and sort key."""
    with open(os.path.expanduser(filename), 'w') as outstream:
        writer = csv.DictWriter(outstream, fieldnames=header)
        writer.writeheader()
        for row in sorted(data, key=sort_key):
            writer.writerow(row)

def onwards(table, date):
    """Returns the rows of the table whose dates are at least the given date."""
    return [row
            for row in table
            if row['date'] >= date]

def until(table, date):
    """Returns the rows of the table whose dates are at most the given date."""
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
