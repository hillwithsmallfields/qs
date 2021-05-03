#!/usr/bin/python3

import argparse
import csv

import canonical_sheet
import named_column_sheet
import qsutils

item_detail_columns = [
    'date',
    'seller',
    'item_name',
    'price',
    'p_and_p_price',
    'quantity',
    'item_total',
    'message_number']

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("--detail", action='append')
    parser.add_argument("--update")
    parser.add_argument("--input")
    parser.add_argument("--output")
    args = parser.parse_args()
    
    if not args.input and not args.update:
        print("At least one of --input and --update must be given")
        return
    if args.input and args.update:
        print("Only one of --input and --update must be given")
        return
    if not args.update and not args.output:
        print("At least one of --update and --output must be given")
        return
    if args.update and args.output:
        print("Only one of --update and --output must be given")
        return

    config = qsutils.program_load_config(args)

    mainsheet = canonical_sheet.canonical_sheet(config, input_sheet=args.input or args.update, convert_all=True)

    details = named_column_sheet.named_column_sheet(config, item_detail_columns)
    
    for more_details in args.detail:
        with open(more_details) as detail_stream:
            for row in csv.DictReader(detail_stream):
                details.add_row(row)

    by_date = {}

    for dk, dv in details.rows.items():
        row_date = dk.date()
        if row_date not in by_date:
            by_date[row_date] = {}
        by_date[row_date][dv['seller'].split(' ')[0].lower()] = dv

    modified = 0

    for tk, tv in mainsheet.rows.items():
        t_date = tk.date()
        if t_date in by_date:
            t_payee = tv['payee'].split(' ')[0].lower()
            if t_payee in by_date[t_date]:
                if tv.get('category') == 'Postage':
                    continue
                if by_date[t_date][t_payee].get('used'):
                    print("on date", t_date, "with payee", t_payee, "in category", tv['category'], "already used", by_date[t_date][t_payee]['item_name'])
                    continue
                tv['item'] = by_date[t_date][t_payee]['item_name']
                by_date[t_date][t_payee]['used'] = tv
                modified += 1

    print(len(mainsheet.rows), "main rows", len(details.rows), "detail rows", modified, "entries modified")

    for when in sorted(by_date.keys()):
        what = by_date[when]
        date_printed = False
        for who in sorted(what.keys()):
            item_row = what[who]
            if not item_row.get('used', None):
                if not date_printed:
                    print("On", when)
                    date_printed = True
                print("  ", who if who != "" else "unknown", "sold", item_row['item_name'])
    
    mainsheet.write_csv(args.update or args.output, suppress_timestamp=True)
    
if __name__ == '__main__':
    main()
