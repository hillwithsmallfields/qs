#!/usr/bin/python3

import canonical_sheet
import csv
import qsutils

def mark_transfers_begin(control_dict, input_format):
    return canonical_sheet.canonical_sheet.canonical_column_sequence, {'previous': None}

def mark_transfers_row(timestamp, row, output_rows, scratch):
    previous = scratch['previous']
    if previous:
        amount = float(row['amount'])
        if amount == - float(previous['amount']) and row['account'] != previous['account']:
            prev_to_this = bool(amount > 0)
            to_row = row if prev_to_this else previous
            from_row = previous if prev_to_this else row
            row_date = row['date']
            row_time = row['time']
            print(abs(amount), "from", from_row['account'], "to", to_row['account'], "on", row_date, "between", row_time, "and", previous['time'])
            if previous['date'] != row_date:
                print("date mismatch")
    scratch['previous'] = row

def mark_transfers_end(headers, output_rows, scratch):
    return headers, output_rows

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("-o", "--output")
    parser.add_argument("input_file")
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    with open(args.input_file) as instream:
        qsutils.process_fin_csv({'args': args,
                                 'config': config,
                                 'format': None},
                                mark_transfers_begin,
                                mark_transfers_row,
                            mark_transfers_end)

if __name__ == "__main__":
    main()
