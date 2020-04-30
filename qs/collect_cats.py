#!/usr/bin/python3

import qsutils

def collect_cats_begin(control_dict, input_format):
    return ['category', 'parent'], {'parentage': {},
                                    'controls': control_dict,
                                    'input_format': input_format}

def collect_cats_row(timestamp, row, output_rows, scratch):
    if 'category' in row and 'parent' in row:
        scratch['parentage'][row['category']] = row['parent']

def collect_cats_end(headers, output_rows, scratch):
    for cat, parent in scratch['parentage'].items():
        output_rows[cat] = {'category': cat, 'parent': parent}
    return headers, output_rows

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("-o", "--output")
    parser.add_argument("input_file")
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    qsutils.process_fin_csv({'args': args,
                             'config': config,
                             'format': None},
                            collect_cats_begin,
                            collect_cats_row,
                            collect_cats_end)

if __name__ == "__main__":
    main()
