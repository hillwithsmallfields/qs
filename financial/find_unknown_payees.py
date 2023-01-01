#!/usr/bin/env python3

import argparse

import finutils

def find_unknown_payees(incoming, conversions):
    conversions = finutils.read_yaml(conversions)['formats']['Default']['conversions']
    for payee in sorted({trimmed
                         for trimmed in [finutils.without_numeric_tail(row['Details'])
                                         for row in finutils.read_csv(incoming)]
                         if trimmed not in conversions}):
        print(payee)

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--incoming", "-i", default=finutils.BANK_BASE)
    parser.add_argument("--conversions", "-c", default=finutils.CONVERSIONS)
    return vars(parser.parse_args())

if __name__ == "__main__":
    find_unknown_payees(**get_args())
