#!/usr/bin/env python3

import argparse
from collections import defaultdict

import finutils

def find_unknown_payees(incoming, conversions):
    unknowns = defaultdict(list)
    for row in incoming:
        trimmed = finutils.without_numeric_tail(row['Details'])
        if trimmed not in conversions:
            unknowns[trimmed].append(row)
    return unknowns

def find_unknown_payees_in_files(incoming, conversions):
    unknowns = find_unknown_payees(finutils.read_csv(incoming),
                                   finutils.read_yaml(conversions)['formats']['Default']['conversions'])
    for u in sorted(unknowns.keys()):
        print(u)
        for d in sorted(unknowns[u], key=lambda r: r['Date']):
            print("  ", d['Date'], float(d['Money in'] or 0) - float(d['Money out'] or 0))

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--incoming", "-i", default=finutils.BANK_BASE)
    parser.add_argument("--conversions", "-c", default=finutils.CONVERSIONS)
    return vars(parser.parse_args())

if __name__ == "__main__":
    find_unknown_payees_in_files(**get_args())
