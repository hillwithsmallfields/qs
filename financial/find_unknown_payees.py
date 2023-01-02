#!/usr/bin/env python3

import argparse
from collections import defaultdict

import titlecase

import finutils

def find_unknown_payees(incoming, conversions):
    unknowns = defaultdict(list)
    for row in incoming:
        trimmed = finutils.without_numeric_tail(row['Details'])
        print(trimmed)
        if trimmed not in conversions:
            unknowns[trimmed].append(row)
    return unknowns

def find_unknown_payees_in_files(incoming, conversions, output, verbose):
    unknowns = find_unknown_payees(finutils.read_csv(incoming),
                                   finutils.read_yaml(conversions)['formats']['Default']['conversions'])
    if verbose or not output:
        for u in sorted(unknowns.keys()):
            print(u)
            for d in sorted(unknowns[u], key=lambda r: r['Date']):
                print("  ", d['Date'], float(d['Money in'] or 0) - float(d['Money out'] or 0))
    if output:
        with open(output, 'w') as outstream:
            for u in sorted(unknowns.keys()):
                outstream.write("      {}:\n".format(u))
                outstream.write("        category: \n")
                outstream.write("        payee: {}:\n".format(titlecase.titlecase(u)))

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--incoming", "-i", default=finutils.BANK_BASE)
    parser.add_argument("--conversions", "-c", default=finutils.CONVERSIONS)
    parser.add_argument("--output", "-o")
    parser.add_argument("--verbose", "-v", action='store_true')
    return vars(parser.parse_args())

if __name__ == "__main__":
    find_unknown_payees_in_files(**get_args())
