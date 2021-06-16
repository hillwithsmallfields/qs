#!/usr/bin/python3

import argparse
import csv
import datetime
import glob
import re

dated_line = re.compile("^([0-9][0-9][A-Z][A-Z][A-Z][0-9][0-9])  +\*?(.+)")
details_line = re.compile("^(.+) +([0-9,]+\.[0-9][0-9])$")
splitter = re.compile("  +")
bump = datetime.timedelta(seconds=60)
# trailer_patterns = [re.compile(tp) for tp in ("Please refer any enquiries", "NO ACCOUNT MOVEMENT SINCE", "Your deposit is eligible")]
numeric = re.compile("[-0-9,.]+")

def is_trailer(line):
    for tp in trailer_patterns:
        if tp.match(line):
            return True
    return False

def fnumber(string):
    return float(string.replace(',', '')) if isinstance(string, str) and numeric.match(string) else string

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", "-o", default="statements.csv")
    parser.add_argument("inputs", nargs='*')
    args = parser.parse_args()

    transactions = {}

    for infile in args.inputs:
        entry = None
        with open(infile) as instream:
            for line in instream:
                line = line.strip()
                if line == "":
                    continue
                matched = dated_line.match(line)
                if matched:
                    line_date = datetime.datetime.strptime(matched.group(1), "%d%b%y")
                    raw_parts = splitter.split(matched.group(2))
                    line_parts = {line.index(part)+len(part): fnumber(part) for part in raw_parts[1:]}
                    entry = [line_date, raw_parts[0].capitalize(), line_parts]
                elif entry:
                    matched = details_line.match(line)
                    if matched:
                        entry += [matched.group(1).strip(), fnumber(matched.group(2))]
                    if line_date in transactions and transactions[line_date] == entry:
                        print("looks like a possible duplicate, file:", infile, "line:", line)
                        entry = None
                        continue
                    while line_date in transactions:
                        line_date += bump
                    transactions[line_date] = entry
                    entry = None
    columns = set()
    for when in sorted(transactions.keys()):
        what = transactions[when]
        columns |= set(what[2].keys())
    # for column in sorted(columns):
    #     print("column", column, "is used")
    with open(args.output, 'w') as outstream:
        writer = csv.DictWriter(outstream, ('Date', 'Description', 'Amount', 'Balance'))
        writer.writeheader()
        for when in sorted(transactions.keys()):
            what = transactions[when]
            parts = what[2]
            description = what[1]
            if len(what) >= 4:
                description += "; " + what[3]
            writer.writerow({'Date': when.isoformat(),
                             'Description': description,
                             'Amount': parts.get(75, 0) - parts.get(53, 0),
                             'Balance': parts.get(98, what[4] if len(what) >= 5 else '')})

if __name__ == '__main__':
    main()
