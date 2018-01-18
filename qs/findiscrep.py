#!/usr/bin/python

# Program to track discrepancies in finance spreadsheets

import argparse
import csv
import os
import qsutils

# See notes in finconv.py for config file format

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def find_discrepancies(args, config, input_format, rows):
    date_element_index = 1 # 0 = year, 1 = month, 2 = day # todo: make this a command-line option
    comparisons = input_format['comparisons'].keys()
    outputs = ['date']
    for comp in comparisons:
        outputs += [ comp + '-accum', comp + '-delta' ]
    output_rows = {}
    print "Outputs will be", outputs
    comparing_values = {}
    comparing_dates = {}
    date_column_name = input_format['columns']['date']
    for timestamp in sorted(rows.keys()):
        row = rows[timestamp]
        output_row = {}
        for comp in comparisons:
            if comp in row:
                raw = row[comp]
                if raw == '':
                    continue
                row_date = row[date_column_name].split('-')
                significant_date = row_date[date_element_index]
                print "Got comparison", raw, "for", comp, "on", row_date
                number = float(raw)
                if comp in comparing_values:
                    if significant_date != comparing_dates[comp]:
                        print "date", significant_date, "changed since", comparing_dates[comp]
                        print "value changed from", comparing_values[comp], "to", number
                        change = comparing_values[comp] - number
                        print "change is", change
                        comparing_dates[comp] = significant_date
                        comparing_values[comp] = number
                        output_row[comp + '-accum'] = number
                        output_row[comp + '-delta'] = change
                else:
                    comparing_values[comp] = number
                    comparing_dates[comp] = significant_date
        if len(output_row) > 0:
            row_date = row[date_column_name]
            output_row['date'] = row_date
            print "made output row", output_row
            output_rows[row_date] = output_row
    return outputs, output_rows

def process_fin_csv(args, config, callback):
    with open(os.path.expanduser(os.path.expandvars(args.input_file))) as infile:
        if args.format and (args.format in config['formats']):
            input_format_name = args.format
        else:
            input_format_name, header_row_number = qsutils.deduce_stream_format(infile, config, args.verbose)

        input_format = config['formats'][input_format_name]

        in_columns = input_format['columns']
        column_defaults = input_format.get('column-defaults', {})
        in_date_column = in_columns['date']
        in_time_column = in_columns.get('time', None)

        if args.verbose:
            print "Reading", os.path.expanduser(os.path.expandvars(args.input_file)), "as format", input_format_name

        rows = {}
        header_row_number = 0
        for _ in range(1, header_row_number):
            dummy = infile.readline()
        for row in csv.DictReader(infile):
            row = {k:v for k,v in row.iteritems() if k != ''}
            row_date = qsutils.normalize_date(row[in_date_column])
            row_time = row[in_time_column] if in_time_column else column_defaults.get('time', "01:02:03")
            row_timestamp = row_date+"T"+row_time
            rows[row_timestamp] = row
        print "got", len(rows), "rows"
        header, output_rows = callback(args, config, input_format, rows)
        if len(rows) > 0:
            with open(os.path.expanduser(os.path.expandvars(args.output)), 'w') as outfile:
                writer = csv.DictWriter(outfile, header)
                writer.writeheader()
                for timestamp in sorted(output_rows.keys()):
                    writer.writerow({ k: (("%.2F" % v)
                                          if type(v) is float
                                          else v)
                                      for k, v in output_rows[timestamp].iteritems()})


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append')
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')
    parser.add_argument("-o", "--output")
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("input_file")

    args = parser.parse_args()

    config_files = ([DEFAULT_CONF]
                    if os.path.exists(DEFAULT_CONF) and not args.no_default_config
                    else [])

    if args.config:
        config_files += args.config

    config = qsutils.load_config(args.verbose, *config_files)

    process_fin_csv(args, config, find_discrepancies)

if __name__ == "__main__":
    main()
