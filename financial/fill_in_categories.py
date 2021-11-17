#!/usr/bin/python3

import argparse
import canonical_sheet
import csv
import os
import qsutils

def read_category_table(category_file):
    category_parentage = {}
    with open(category_file) as cats_in:
        for row in csv.DictReader(cats_in):
            category_parentage[row['category']] = row['parent']
    return category_parentage

def main():
    parser = qsutils.program_argparser()
    parser.add_argument('--category-parentage', '-C',
                        help="""The file containing the parentage definitions.""")
    parser.add_argument('--output', '-o',
                        help="""The file to write the results to, if not the original file.""")
    parser.add_argument('filename')
    args = parser.parse_args()
    config = qsutils.program_load_config(args, quiet=True)
    categories = read_category_table(args.category_parentage
                                     or os.path.expandvars("$SYNCED/finances/categories.csv"))
    print("reading", args.filename)
    sheet = canonical_sheet.canonical_sheet(config,
                                            input_sheet=args.filename,
                                            convert_all=True)
    print("sheet is", sheet)
    for timestamp, row in sheet.rows.items():
        old_parentage = row.get('parent', None)
        new_parentage = categories.get(row['category'])
        if old_parentage != new_parentage:
            if old_parentage and new_parentage:
                print("Reparenting transaction with", row['payee'], "on", row['date'], "from", old_parentage, "to", new_parentage)
        if new_parentage:
            row['parent'] = new_parentage
    sheet.write_csv(args.output or args.filename, suppress_timestamp=True)

if __name__ == "__main__":
    main()
