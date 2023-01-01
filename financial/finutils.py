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

def read_csv(filename):
    with open(os.path.expanduser(filename)) as instream:
        return set(frozendict(row) for row in csv.DictReader(instream))

def write_csv(data, header, filename, sort_key):
    with open(os.path.expanduser(filename), 'w') as outstream:
        writer = csv.DictWriter(outstream, fieldnames=header)
        writer.writeheader()
        for row in sorted(data, key=sort_key):
            writer.writerow(row)

def read_yaml(filename):
    with open(os.path.expanduser(filename)) as instream:
        return yaml.safe_load(instream)

def without_numeric_tail(string):
    matched = re.match("^[^-0-9]+", string)
    return matched.group(0).strip().lstrip(" *") if matched else string

def show_sample(table, n_samples=12):
    table = sorted(table, key=lambda r: (r.get('Date', r.get('date')), r'[Details'))
    for i in range(0, len(table), len(table)//n_samples):
        print(i, table[i])
