#!/usr/bin/python3

import argparse
import csv
import os

def date_set(filename):
    result = set()
    with open(os.path.expanduser(filename)) as instream:
        for row in csv.DictReader(instream):
            result.add(row['Date'])
    return result

def check_merged_row_dates(combined, *originals):
    originals_dates = set()
    for original in originals:
        originals_dates |= date_set(original)
    return len(date_set(combined)) == len(originals_dates)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("original")
    parser.add_argument("others", nargs='+')
    args = parser.parse_args()

    if check_merged_row_dates(args.original, *args.others):
        print("OK")
        exit(0)
    else:
        print("not ok")
        exit(1)

if __name__ == '__main__':
    main()
