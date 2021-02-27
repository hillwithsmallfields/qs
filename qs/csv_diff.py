#!/usr/bin/python3

import argparse
import csv

def safe_float(x):
    try:
        return float(x)
    except Exception:
        return x

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", "-o")
    parser.add_argument("a")
    parser.add_argument("b")
    args = parser.parse_args()
    with open(args.a) as instream:
        sheet_a = [row for row in csv.DictReader(instream)]
    with open(args.b) as instream:
        sheet_b = [row for row in csv.DictReader(instream)]
    index_a = 0
    index_b = 0
    while True:
        row_a = sheet_a[index_a]
        row_b = sheet_b[index_b]
        for column in set(list(row_a.keys()) + list(row_b.keys())):
            cell_a = row_a.get(column, "") or None
            cell_b = row_b.get(column, "") or None
            if safe_float(cell_a) != safe_float(cell_b):
                print("%d:%d:%s: %s!=%s" % (index_a, index_b, column, cell_a, cell_b))
        # Fortunately I didn't need to handle missing lines in this
        # application, but I might need to do that later:
        index_a += 1
        if index_a >= len(sheet_a):
            break
        index_b += 1
        if index_b >= len(sheet_b):
            break
    
if __name__ == '__main__':
    main()
