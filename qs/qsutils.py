#!/usr/bin/python
# Common routines for my QS programs

import csv
import os
import pprint
import re
import yaml

def deduce_file_type_from_headers(headers):
    if 'Kg' in headers:
        return 'weight'
    if 'Currency' in headers:
        return 'finances'
    return 'unknown'

# based on https://stackoverflow.com/questions/3232943/update-value-of-a-nested-dictionary-of-varying-depth
def rec_update(d, u, i=""):
    for k, v in u.iteritems():
        if isinstance(v, dict):
            d[k] = rec_update(d.get(k, {}), v, "  ")
        elif isinstance(v, list):
            d[k] = d.get(k, []) + [(ve if ve != 'None' else None) for ve in v]
        elif v == 'None':
            d[k] = None
        else:
            d[k] = v
    return d

def load_config(verbose, *config_files):
    config = {}
    for file in config_files:
        with open(os.path.expanduser(os.path.expandvars(file))) as config_file:
            more_config = yaml.safe_load(config_file)
            rec_update(config, more_config)
    if verbose:
        print "Read config:"
        print yaml.dump(config)
    return config

def deduce_format(first_row, formats, verbose=False):
    # take out some strange characters that Financisto is putting in
    condensed_row = [cell.lstrip("\357\273\277") for cell in first_row if cell != ""]
    if verbose:
        print "Trying to deduce format using sample row", condensed_row
    for format_name, format_def in formats.iteritems():
        sequence = [col for col in format_def['column-sequence'] if col]
        if verbose:
            print "  Comparing with", format_name, "sample row", sequence
        if sequence == condensed_row:
            if verbose:
                print "Format seems to be", format_name
            return format_name
    if verbose:
        print "Could not deduce format"
    return None

def deduce_stream_format(infile, config, verbose):
    sampling_countdown = 12
    input_format_name = None
    header_row_number = 0
    for sample_row in csv.reader(infile):
        header_row_number += 1
        input_format_name = deduce_format(sample_row,
                                          config['formats'],
                                          verbose)
        if input_format_name:
            break
        sampling_countdown -= 1
        if sampling_countdown <= 0:
            if verbose:
                print "Giving up on deducing format"
            break;
    infile.seek(0)
    return input_format_name, header_row_number

ISO_DATE = re.compile("[0-9]{4}-[0-9]{2}-[0-9]{2}")
SLASHED_DATE = re.compile("[0-9]{4}/[0-9]{2}/[0-9]{2}")

def normalize_date(date_in):
    if ISO_DATE.match(date_in):
        return date_in
    if SLASHED_DATE.match(date_in):
        return date_in.replace('/', '-')
    return date_in

def process_fin_csv(args, config, callback, *callbackextraargs):
    """Process a CSV file in one of my financial formats.
    From an application, you should probably call process_fin_csv_rows
    instead of this."""
    expanded_input_name = os.path.expanduser(os.path.expandvars(args.input_file))
    with open(expanded_input_name) as infile:
        if args.format and (args.format in config['formats']):
            input_format_name = args.format
        else:
            input_format_name, header_row_number = deduce_stream_format(infile, config, args.verbose)

        input_format = config['formats'][input_format_name]

        in_columns = input_format['columns']
        column_defaults = input_format.get('column-defaults', {})
        in_time_column = in_columns.get('time', None)
        invert_columns = { v:k for k, v in in_columns.iteritems() }

        if args.verbose:
            print "Reading", expanded_input_name, "as format", input_format_name

        rows = {}
        header_row_number = 0
        for _ in range(1, header_row_number):
            dummy = infile.readline()
        for row in csv.DictReader(infile):
            row = {k:v for k,v in row.iteritems() if k != ''}
            for column in invert_columns:
                if column in row: # canonicalize the names
                    row[invert_columns[column]] = row[column]
            row_date = normalize_date(row['date'])
            row_time = row.get('time', column_defaults.get('time', "01:02:03"))
            row_timestamp = row_date+"T"+row_time
            rows[row_timestamp] = row
        if args.verbose:
            print "Read", len(rows), "rows from", expanded_input_name
        header, output_rows = callback(args, config, input_format, rows, *callbackextraargs)
        if output_rows and len(output_rows) > 0:
            expanded_output_name = os.path.expanduser(os.path.expandvars(args.output))
            with open(expanded_output_name, 'w') as outfile:
                writer = csv.DictWriter(outfile, header)
                writer.writeheader()
                for timestamp in sorted(output_rows.keys()):
                    writer.writerow({ k: (("%.2F" % v)
                                          if type(v) is float
                                          else v)
                                      for k, v in output_rows[timestamp].iteritems()})
                if args.verbose:
                    print "Wrote", len(output_rows), "rows to", expanded_output_name

def process_rows(args, config, input_format,
                 rows,
                 setup_callback, row_callback, tidyup_callback):
    """Process CSV rows.

From an application, you should probably call process_fin_csv_rows
instead of this.

The setup_callback must take the args structure (from argparse), the
config dictionary tree, and the input_format, and return a list of
columns wanted in the output, and a scratch data value (normally a
dictionary) for use in the row handler and the tidy_up function.

The row handler must take the row timestamp, the row data (as a
dictionary), a dictionary to fill in with the output rows (it will be
output in the order of its keys), and the scratch data.  In the row
dictionary it is given, the indirections given in the `columns' part
of the input format will have been done, for example if the format
specifies `payee: Details', the `Details' column will have been copied
into `payee'.

The tidy_up function must take, and return, the header list and the
output rows dictionary, and also take the scratch data.  It should not
do the output; that will be done by this framework.  Alternatively, it
can do the output itself and return None, None to suppress the normal
output.

    """
    column_headers, scratch = (setup_callback(args, config,
                                             input_format)
                               if setup_callback
                               else ([],{}))
    output_rows = {}
    for timestamp in sorted(rows.keys()):
        row_callback(timestamp, rows[timestamp], output_rows, scratch)
    if tidyup_callback:
        column_headers, output_rows = tidyup_callback(column_headers, output_rows, scratch)
    return column_headers, output_rows

def process_fin_csv_rows_fn(args, config, input_format, rows, setup_callback, row_callback, tidyup_callback):
    """For internal use by process_fin_csv_rows."""
    return process_rows(args, config, input_format,
                        rows,
                        setup_callback, row_callback, tidyup_callback)

def process_fin_csv_rows(args, config, setup_callback, row_callback, tidyup_callback):
    """See process_rows for descriptions of the callbacks.
    This is the main entry point in this module."""
    return process_fin_csv(args, config,
                           process_fin_csv_rows_fn,
                           setup_callback, row_callback, tidyup_callback)

def main():
    """Tests on the utilities"""
    a = {"one": 1, "two": 2, "three": 3, "teens": {"thirteen": 13, "fourteen": 14}, "listing": ["aon", "do", "tri"]}
    b = {"four": 4, "five": 5, "six": 6, "teens": {"fifteen": 15, "sixteen": 16}, "listing": ["caithair", "cuig", "se"]}
    print "a is", a
    print "b is", b
    rec_update(a, b, "")
    print "a is now", a
    with open("/home/jcgs/qsconf/accounts.yaml") as confile:
        config = yaml.safe_load(confile)
    with open("/home/jcgs/qsconf/conversions.yaml") as confile:
        conversions = yaml.safe_load(confile)
    print "config is:"
    print pprint.pformat(config)
    print "conversions are:"
    print pprint.pformat(conversions)
    rec_update(config, conversions)
    print "overall config is:"
    print pprint.pformat(config)

if __name__ == "__main__":
    main()
