#!/usr/bin/env python3

import csv
import os.path
import re
import yaml

def read_csv(filename):
    with open(os.path.expanduser(filename)) as instream:
        return list(csv.DictReader(instream))

def read_yaml(filename):
    with open(os.path.expanduser(filename)) as instream:
        return yaml.safe_load(instream)

def show_sample(table, n_samples=12):
    for i in range(0, len(table), len(table)//n_samples):
        print(i, table[i])

def without_numeric_tail(string):
    matched = re.match("^[^-0-9]+", string)
    return matched.group(0).strip().lstrip(" *") if matched else string

def convert_bank_row(row, conversions):
    trimmed = without_numeric_tail(row['Details'])
    conversion = conversions.get(trimmed, {})
    return {
        'account': "Handelsbanken current account",
        'payee': conversion.get('payee', trimmed),
        'category': conversion.get('category', 'unknown category'),
        'amount': float(row.get('Money in') or 0) - float(row.get('Money out') or 0)
    }

def convert_bank_table(table, conversions):
    return [convert_bank_row(row, conversions) for row in table]

def finances_update():
    conversions = read_yaml("~/Sync/finances/conversions.yaml")['formats']['Default']['conversions']
    main = read_csv("~/Sync/finances/finances.csv")
    bank = read_csv("~/Sync/finances/handelsbanken/handelsbanken-full.csv")
    converted = convert_bank_table(bank, conversions)
    print("main")
    show_sample(main)
    print("bank")
    show_sample(bank)
    print("converted")
    show_sample(converted)

if __name__ == "__main__":
    finances_update()
