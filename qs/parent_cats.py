#!/usr/bin/python3

import canonical_sheet
import qsutils

def parent_cats_begin(control_dict, input_format):
    return canonical_sheet.canonical_column_sequence, {'controls': control_dict,
                                                       'input_format': input_format}

def parent_cats_row(timestamp, row, output_rows, scratch):
    if ('category' in row and row.get('parent', "") == ""):
        row['parent'] = scratch['parentage'][row['category']]
    output_rows[timestamp] = row

def parent_cats_end(headers, output_rows, scratch):
    return headers, output_rows

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("-o", "--output")
    parser.add_argument("-p", "--parentage")
    parser.add_argument("input_file")
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    with open(args.parentage) as instream:
        qsutils.process_fin_csv({'args': args,
                                 'config': config,
                                 'format': None,
                                 'parentage': {row['category']: row['parent']
                                               for row in csv.DictReader(instream)}},
                                parent_cats_begin,
                                parent_cats_row,
                            parent_cats_end)

if __name__ == "__main__":
    main()
