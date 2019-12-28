#!/usr/bin/python
# Common routines for my QS programs

import argparse
import csv
import datetime
import os
import pprint
import re
import yaml

def trim_if_float(val):
    return (("%.2F" % val)
            if type(val) is float
            else val)

def granularity_day(overprecise):
    return datetime.datetime(overprecise.year, overprecise.month, overprecise.day)

def granularity_month(overprecise):
    return datetime.datetime(overprecise.year, overprecise.month, 1)

def deduce_file_type_from_headers(headers):
    if 'Kg' in headers:
        return 'weight'
    if 'Currency' in headers:
        return 'finances'
    return 'unknown'

def resolve_filename(filename, directory=None):
    """Try to get an absolute form of a filename, using a suggested directory."""
    filename = os.path.expandvars(filename)
    return (filename
            if os.path.isabs(filename)
            else os.path.join(os.path.expanduser(directory or os.getcwd()),
                              os.path.expanduser(filename)))

# based on https://stackoverflow.com/questions/3232943/update-value-of-a-nested-dictionary-of-varying-depth
def rec_update(d, u, i=""):
    for k, v in u.items():
        if isinstance(v, dict):
            d[k] = rec_update(d.get(k, {}), v, "  ")
        elif isinstance(v, list):
            d[k] = d.get(k, []) + [(ve if ve != 'None' else None) for ve in v]
        elif v == 'None':
            d[k] = None
        else:
            d[k] = v
    return d

def string_to_bool(string):
    if string in ['yes', 'Yes', 'YES', 'true', 'true', 'TRUE', '1', True, 1]:
        return True
    if string in ['no', 'no', 'NO', 'false', 'false', 'FALSE', '0', '', None, False, 0]:
        return False
    print("Value", string, "not understood as boolean, treating as False")
    return False

def load_multiple_yaml(target_dict, suggested_dir, yaml_files):
    for yaml_file in yaml_files:
        if yaml_file is None:
            continue
        filename = resolve_filename(yaml_file, suggested_dir)
        if os.path.exists(filename):
            with open(filename) as yaml_handle:
                rec_update(target_dict, yaml.safe_load(yaml_handle))

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def load_config(verbose, base_config, suggested_dir, *config_files):
    """Load config files.
    You can give None and it will be skipped."""
    if base_config is None:
        base_config = {}
    load_multiple_yaml(base_config, suggested_dir, config_files)
    if verbose:
        print("Read config:")
        print(yaml.dump(base_config))
    return base_config

def deduce_format(first_row, formats, verbose=False):
    # take out some strange characters that Financisto is putting in
    condensed_row = [cell.lstrip("\357\273\277") for cell in first_row if cell != ""]
    if verbose:
        print("Trying to deduce format using sample row", condensed_row)
    for format_name, format_def in formats.items():
        sequence = [col for col in format_def['column-sequence'] if col]
        if verbose:
            print("  Comparing with", format_name, "sample row", sequence)
        if sequence == condensed_row:
            if verbose:
                print("Format seems to be", format_name)
            return format_name
    if verbose:
        print("Could not deduce format")
    return None

def deduce_stream_format(infile, config, verbose=False):
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
                print("Giving up on deducing format")
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

later = datetime.timedelta(0, 1)

def read_fin_csv(args, config, input_filename):
    """Read an account spreadsheet file."""
    expanded_input_name = os.path.expanduser(os.path.expandvars(input_filename))
    with open(expanded_input_name) as infile:
        if args.format and (args.format in config['formats']):
            input_format_name = args.format
        else:
            input_format_name, header_row_number = deduce_stream_format(infile, config, args.verbose)

        input_format = config['formats'][input_format_name]

        in_columns = input_format['columns']
        column_defaults = input_format.get('column-defaults', {})
        in_time_column = in_columns.get('time', None)
        invert_columns = { v:k for k, v in in_columns.items()
                           # temporarily: skip columns which have a more complex description; todo: fix this
                           if isinstance(v, basestring) }

        if args.verbose:
            print("Reading", expanded_input_name, "as format", input_format_name)

        rows = {}
        header_row_number = 0
        for _ in range(1, header_row_number):
            dummy = infile.readline()
        for row in csv.DictReader(infile):
            row = {k:v for k,v in row.items() if k != ''}
            for column in invert_columns:
                if column in row: # canonicalize the names
                    row[invert_columns[column]] = row[column]
            row_date = normalize_date(row['date'])
            row_time = row.get('time', column_defaults.get('time', "01:02:03"))
            row_timestamp = row_date+"T"+row_time
            # separate out rows that came in with the same timestamp
            while row_timestamp in rows:
                row_timestamp = (datetime.datetime.strptime(row_timestamp, "%Y-%m-%dT%H:%M:%S")+later).isoformat()
            rows[row_timestamp] = row
        if args.verbose:
            print("Read", len(rows), "rows from", expanded_input_name)
    return input_format, rows

def write_fin_csv(header, output_rows, filename):
    """Write an account spreadsheet file.
    Returns the name of the file it was written to."""
    expanded_output_name = os.path.expanduser(os.path.expandvars(filename))
    with open(expanded_output_name, 'w') as outfile:
        writer = csv.DictWriter(outfile, header)
        writer.writeheader()
        for timestamp in sorted(output_rows.keys()):
            writer.writerow({ k: (("%.2F" % v)
                                  if type(v) is float
                                  else v)
                              for k, v in output_rows[timestamp].items()})
    return expanded_output_name

def read_process_write_fin_csv(app_data, callback, *callbackextraargs):
    """Process a CSV file in one of my financial formats.
    From an application, you should probably call process_fin_csv
    instead of this."""
    input_format, rows = read_fin_csv(app_data['args'], app_data['config'],
                                      app_data['args'].input_file)
    header, output_rows = callback(app_data, input_format, rows, *callbackextraargs)
    if output_rows and len(output_rows) > 0:
        expanded_output_name = write_fin_csv(header, output_rows, app_data['args'].output)
        if app_data['args'].verbose:
            print("Wrote", len(output_rows), "rows to", expanded_output_name)
    elif app_data['args'].verbose:
        print("Nothing to write")

def process_rows(app_data, input_format,
                 rows,
                 setup_callback, row_callback, tidyup_callback):
    """Process CSV rows.

    For using this as the core of an application that uses files for
    its input and output, you should probably call process_fin_csv
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
    column_headers, scratch = (setup_callback(app_data, input_format)
                               if setup_callback
                               else ([],{}))
    output_rows = {}
    for timestamp in sorted(rows.keys()):
        row_callback(timestamp, rows[timestamp], output_rows, scratch)
    if tidyup_callback:
        column_headers, output_rows = tidyup_callback(column_headers,
                                                      output_rows,
                                                      scratch)
    return column_headers, output_rows

def process_fin_csv(app_data, setup_callback, row_callback, tidyup_callback):
    """See process_rows for descriptions of the callbacks.
    This is the main entry point in this module."""
    return read_process_write_fin_csv(app_data,
                                      process_rows,
                                      setup_callback,
                                      row_callback,
                                      tidyup_callback)

def program_argparser():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append',
                        help="""Extra config file (may be given multiple times).""")
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')
    return parser

def program_load_config(args):
    return load_config(args.verbose,
                       None,
                       None,
                       DEFAULT_CONF if not args.no_default_config else None,
                       *args.config or [])

def main():
    """Tests on the utilities"""
    a = {"one": 1, "two": 2, "three": 3, "teens": {"thirteen": 13, "fourteen": 14}, "listing": ["aon", "do", "tri"]}
    b = {"four": 4, "five": 5, "six": 6, "teens": {"fifteen": 15, "sixteen": 16}, "listing": ["caithair", "cuig", "se"]}
    print("a is", a)
    print("b is", b)
    rec_update(a, b, "")
    print("a is now", a)
    with open("/home/jcgs/qsconf/accounts.yaml") as confile:
        config = yaml.safe_load(confile)
    with open("/home/jcgs/qsconf/conversions.yaml") as confile:
        conversions = yaml.safe_load(confile)
    print("config is:")
    print(pprint.pformat(config))
    print("conversions are:")
    print(pprint.pformat(conversions))
    rec_update(config, conversions)
    print("overall config is:")
    print(pprint.pformat(config))

if __name__ == "__main__":
    main()
