#!/usr/bin/env python3

import argparse
from collections import defaultdict
import csv

import finutils

def find_unknown_payees(incoming, conversions):
    unknowns = defaultdict(list)
    for row in incoming:
        trimmed = finutils.row_details(row)
        if trimmed not in conversions:
            unknowns[trimmed].append(row)
    return unknowns

def find_unknown_payees_in_files(incoming, conversions, output, verbose):
    unknowns = find_unknown_payees(finutils.read_transactions(incoming),
                                   finutils.read_conversions(conversions))
    if verbose or not output:
        for u in sorted(unknowns.keys()):
            print(u)
            for d in sorted(unknowns[u], key=finutils.row_date):
                print("  ", finutils.row_date(d), finutils.row_amount(d))
    if output:
        with open(output, 'w') as outstream:
            if output.endswith(".csv"):
                writer = csv.writer(outstream)
                writer.writerow(["statement", "payee", "category", "flags"])
                for u in sorted(unknowns.keys()):
                    writer.writerow([u, "", "", ""])
            else:
                for u in sorted(unknowns.keys()):
                    outstream.write("      {}:\n".format(u))
                    outstream.write("        category: \n")
                    outstream.write("        payee: {}:\n".format(u.title()))

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--incoming", "-i", default=finutils.BANK_BASE)
    parser.add_argument("--conversions", "-c", default=finutils.CONVERSIONS)
    parser.add_argument("--output", "-o")
    parser.add_argument("--verbose", "-v", action='store_true')
    return vars(parser.parse_args())

if __name__ == "__main__":
    find_unknown_payees_in_files(**get_args())
