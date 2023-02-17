#!/usr/bin/env python3

import argparse

from frozendict import frozendict

import finutils

def convert_bank_row(row, conversions):
    """Convert a row from my bank statement format to my unified account format."""
    trimmed = finutils.row_details(row)
    conversion = finutils.longest_subtext_in_table(trimmed.lower(), conversions)
    amount = finutils.row_amount(row)
    return frozendict({
        'account': "Handelsbanken current account",
        'date': finutils.row_date(row),
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

def merge_bank_to_main(base, incoming, conversions):
    return base | convert_bank_table(incoming, conversions)

def finances_update(base, incoming, output, conversions):
    finutils.write_csv(merge_bank_to_main(finutils.read_transactions(base),
                                          finutils.read_transactions(incoming),
                                          finutils.read_conversions(conversions)),
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
