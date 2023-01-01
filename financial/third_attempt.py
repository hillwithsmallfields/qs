#!/usr/bin/env python3

import argparse
import re

from frozendict import frozendict

import finutils

def without_numeric_tail(string):
    matched = re.match("^[^-0-9]+", string)
    return matched.group(0).strip().lstrip(" *") if matched else string

def convert_bank_row(row, conversions):
    trimmed = without_numeric_tail(row['Details'])
    conversion = conversions.get(trimmed, {})
    amount = float(row.get('Money in') or 0) - float(row.get('Money out') or 0)
    return frozendict({
        'account': "Handelsbanken current account",
        'date': row.get('Date'),
        'time': "00:00:01",
        'payee': conversion.get('payee', trimmed),
        'category': conversion.get('category', 'unknown category'),
        'amount': amount,
        'currency': 'GBP',
        'original_amount': amount,
        'original_currency': 'GBP',
        'statement': float(row.get('Balance', 0)),
    })

def convert_bank_table(table, conversions):
    return set(convert_bank_row(row, conversions) for row in table)

def finances_update(base, incoming, output, conversions):
    finutils.write_csv(finutils.read_csv(base)
                       | convert_bank_table(finutils.read_csv(incoming),
                                            finutils.read_yaml(conversions)['formats']['Default']['conversions']),
                       finutils.MAIN_HEADERS,
                       output,
                       lambda r: (r['date'], r['time'], r'[payee'))

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--base", "-b", default=finutils.MAIN_ACCOUNTING_FILE)
    parser.add_argument("--incoming", "-i", default=finutils.BANK_BASE)
    parser.add_argument("--output", "-o", default=finutils.MAIN_ACCOUNTING_FILE)
    parser.add_argument("--conversions", "-c", default=finutils.CONVERSIONS)
    return vars(parser.parse_args())

if __name__ == "__main__":
    finances_update(**get_args())
