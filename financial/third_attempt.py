#!/usr/bin/env python3

import csv
import os.path
import re
import yaml

from frozendict import frozendict

import finutils

def without_numeric_tail(string):
    matched = re.match("^[^-0-9]+", string)
    return matched.group(0).strip().lstrip(" *") if matched else string

def convert_bank_row(row, conversions):
    trimmed = without_numeric_tail(row['Details'])
    conversion = conversions.get(trimmed, {})
    return frozendict({
        'account': "Handelsbanken current account",
        'date': row.get('Date'),
        'payee': conversion.get('payee', trimmed),
        'category': conversion.get('category', 'unknown category'),
        'amount': float(row.get('Money in') or 0) - float(row.get('Money out') or 0),
        'statement': float(row.get('Balance', 0)),
    })

def convert_bank_table(table, conversions):
    return set(convert_bank_row(row, conversions) for row in table)

def finances_update():
    conversions = finutils.read_yaml("~/Sync/finances/conversions.yaml")['formats']['Default']['conversions']
    main = finutils.read_csv("~/Sync/finances/finances.csv")
    bank = finutils.read_csv("~/Sync/finances/handelsbanken/handelsbanken-full.csv")
    converted = convert_bank_table(bank, conversions)
    print("main")
    finutils.show_sample(main)
    print("bank")
    finutils.show_sample(bank)
    print("converted")
    finutils.show_sample(converted)

if __name__ == "__main__":
    finances_update()
