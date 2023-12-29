#!/usr/bin/python3
# Common routines for my QS programs

import argparse
import calendar
import csv
import datetime
import functools
import operator
import os
import pprint
import re
import yaml

EXCEL_EPOCH = datetime.date(1899, 12, 31)

def excel_date(date1):          # from http://stackoverflow.com/questions/9574793/how-to-convert-a-python-datetime-datetime-to-excel-serial-date-number
    temp = datetime.datetime(1899, 12, 31)
    parts = [int(x) for x in date1.split('-')]
    delta = datetime.datetime(parts[0], parts[1], parts[2]) - temp
    return float(delta.days) + (float(delta.seconds) / 86400)

def ensure_numeric_dates(table):
    for row in table:
        date = row['Date']
        row['Date number'] = ((datetime.date.fromisoformat(date) if isinstance(date, str) else date) - EXCEL_EPOCH).days
    return table

def tidy_for_output(val):
    """Make a value more neatly printable."""
    return (("%.2F" % val)
            if type(val) is float
            else (None
                  if getattr(val, '_hide_in_csv', False)
                  else (""
                        if (val is None or val == "None")
                        else str(val))))

def subtract_cell(row_a, row_b, name):
    if name not in row_a and name not in row_b:
        return None
    a = 0
    b = 0
    try:
        a = float(row_a[name])
    except (KeyError, TypeError, ValueError):
        pass
    try:
        b = float(row_b[name])
    except (KeyError, TypeError, ValueError):
        pass
    return a - b

def subtracted_row(row, other_row, column_names):
    # print("subtracting", other_row, "from", row, "with columns", column_names)
    return {colname: subtract_cell(row, other_row, colname)
            for colname in column_names}

def big_enough(row, colname, threshold):
    if colname not in row:
        return False
    try:
        return abs(float(row[colname])) >= threshold
    except (TypeError, ValueError):
        return False

def thresholded_row(row, threshold):
    return {colname: row[colname]
            for colname in row.keys()
            if colname in row and big_enough(row, colname, threshold)}

def granularity_day(overprecise):
    """Return the start of the day containing a given timestamp."""
    return datetime.datetime(overprecise.year, overprecise.month, overprecise.day)

def granularity_month(overprecise):
    """Return the start of the month containing a given timestamp."""
    return datetime.datetime(overprecise.year, overprecise.month, 1)

def granularity_year(overprecise):
    """Return the start of the year containing a given timestamp."""
    return datetime.datetime(overprecise.year, 1, 1)

def same_day(a, b):
    """Return whether two dates are in the same day."""
    return granularity_day(a) == granularity_day(b)

def same_month(a, b):
    """Return whether two dates are in the same month."""
    return granularity_month(a) == granularity_month(b)

def same_year(a, b):
    """Return whether two dates are in the same year."""
    return granularity_year(a) == granularity_year(b)

def within_days(a, b, days):
    return abs(a - b).days <= days

def duration_string_to_minutes(durstring):
    parts = durstring.split(':')
    return int(parts[0]) * 60 + int(parts[1]) + float(parts[2]) / 60

def string_to_number(number_string):
    "Convert a string to a number, even if it has commas for the thousands separator."
    try:
        return float(number_string.replace(',', ''))
    except ValueError:
        return 0.0

def resolve_filename(filename, directory=None):
    """Try to get an absolute form of a filename, using a suggested directory."""
    filename = os.path.expandvars(filename)
    return (filename
            if os.path.isabs(filename)
            else os.path.join(
                    os.path.expandvars(
                        os.path.expanduser(directory or os.getcwd())),
                    os.path.expandvars(
                        os.path.expanduser(filename))))

def ensure_directory_for_file(filename):
    """Ensure the directory for a file exists, if possible."""
    dirname = os.path.dirname(filename)
    if dirname != '':
        os.makedirs(dirname, exist_ok=True)

def dict_to_string_sorted_by_key(d):
    return "{" + ", ".join([str(k) + ": " + str(d[k]) for k in sorted(d.keys())]) + "}"

main_keys = ['date', 'time', 'payee', 'amount']

def row_as_string_main_keys(row):
    return "{" + ", ".join([str(k) + ": " + str(row.get(k, "?")) for k in main_keys]) + "}"

# based on https://stackoverflow.com/questions/3232943/update-value-of-a-nested-dictionary-of-varying-depth
def rec_update(basedict, u):
    """Update a dictionary recursively."""
    for k, v in u.items():
        if isinstance(v, dict):
            basedict[k] = rec_update(basedict.get(k, {}), v)
        elif isinstance(v, list):
            base = basedict.get(k, [])
            basedict[k] = base + [(ve if ve != 'None' else None)
                                  for ve in v
                                  if ve not in base]
        elif v == 'None':
            basedict[k] = None
        else:
            basedict[k] = v
    return basedict

def string_to_bool(string):
    if string in ['yes', 'Yes', 'YES', 'true', 'true', 'TRUE', '1', True, 1, 'Detected']:
        return True
    if string in ['no', 'no', 'NO', 'false', 'false', 'FALSE', '0', '', None, False, 0, 'Not Detected']:
        return False
    print("Value", string, "not understood as boolean, treating as False")
    return False

def sum_amount(iterable, fieldname='amount'):
    """Sum a field in a collection of dictionaries.
    This can be used for a column in a spreadsheet."""
    return functools.reduce(operator.add,
                            [x[fieldname] for x in iterable],
                            0)

def combine_transactions(a, b):
    """Make a transaction representing two given transactions."""
    # first, get all the fields from both
    ab = a.copy()
    ab.update(b)
    if a.get('currency', None) == b.get(currency, None):
        ab['amount'] = a.get('amount', 0) + b.get('amount', 0)
        if 'original_amount' in a or 'original_amount' in b:
            ab['original_amount'] = a.get('original_amount', 0) + b.get('original_amount', 0)
    for k in ('payee', 'account', 'currency', 'category', 'parent', 'location' 'project', 'message'):
        av = a.get(k, None)
        bv = b.get(k, None)
        if av is not None and bv is not None and av != bv:
            ab[k] = a.get(k, "") + ";" + b.get
    return ab

def merge_by_date(by_timestamp, period):
    """Return a dictionary with the entries in the input combined by date.

    `period' is a function which should return the starting
    datetime.datetime of the period containing the date it is
    given."""
    result = {}
    for k, v in by_timestamp.items():
        kpart = period(k)
        result[kpart] = combine_transactions(result[kpart], v) if kpart in result else v
    return result

def load_multiple_yaml(target_dict, suggested_dir, yaml_files):
    """Load several YAML files, merging the data from them."""
    directories_used = set()
    for yaml_file in yaml_files:
        if yaml_file is None:
            continue
        filename = resolve_filename(yaml_file, suggested_dir)
        if os.path.exists(filename):
            directories_used.add(os.path.dirname(filename))
            with open(filename) as yaml_handle:
                rec_update(target_dict, yaml.safe_load(yaml_handle))
    return directories_used

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def load_config(verbose, base_config, suggested_dir, *config_files):
    """Load config files.
    You can give None and it will be skipped."""
    if base_config is None:
        base_config = {}
    directories_used = load_multiple_yaml(base_config, suggested_dir, config_files)
    base_config['config_dirs'] = list(set(base_config.get('config_dirs', [])) | directories_used)
    if 'equivalents' in base_config:
        equivalents = base_config['equivalents']
        equiv_table = {}
        equiv_reverse_table = {}
        if verbose:
            print("equivalents are", equivalents)
        for equiv_primary, equiv_secondaries in equivalents.items():
            combined = [equiv_primary] + equiv_secondaries
            for e in combined:
                equiv_table[e] = combined
            for sec in equiv_secondaries:
                equiv_reverse_table[sec] = equiv_primary
        if verbose:
            print("equiv_table", equiv_table)
        base_config['equivalents'] = equiv_table
        base_config['reverse-equivalents'] = equiv_reverse_table
        if verbose:
            print("reverse equivalents are", equiv_reverse_table)
    equivalent_names = base_config.get('equivalents', {})
    if verbose:
        print("equivalent_names are originally", equivalent_names)
        print("Read config:")
        print(yaml.dump(base_config))
    return base_config

def combine_configs(conf_a, conf_b):
    return rec_update(rec_update({}, conf_a), conf_b)

def deduce_format(first_row, formats, verbose=False):
    # take out some strange characters that Financisto is putting in
    condensed_row = [cell.lstrip("\357\273\277") for cell in first_row if cell != ""]
    if verbose:
        print("Trying to deduce format using sample row", condensed_row)
    for format_name, format_def in formats.items():
        if 'column-sequence' not in format_def:
            print("Format", format_name, "has no column-sequence")
            continue
        sequence = [col for col in format_def['column-sequence'] if col]
        if verbose:
            print("  Comparing with", format_name, "sample row", sequence)
        if sequence == condensed_row:
            if verbose:
                print("Format seems to be", format_name, "(by headers)")
            return format_name
    for format_name, format_def in formats.items():
        if 'column-patterns' not in format_def:
            continue
        patterns = [col for col in format_def['column-patterns'] if col]
        if verbose:
            print("  Comparing with", format_name, "against patterns", patterns)
        all_matching = True
        for pattern, cell in zip(patterns, condensed_row):
            if not re.match(pattern, cell):
                all_matching = False
                break
        if all_matching:
            if verbose:
                print("Format seems to be", format_name, "(by patterns)")
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

def earliest_unfetched(data):
    return forward_from(max(data.keys()), 0, 0, 1)

def ensure_directory_present_and_empty(directory):
    if os.path.isdir(directory):
        for old_file in os.listdir(directory):
            os.remove(os.path.join(directory, old_file))
    else:
        os.makedirs(directory, exist_ok=True)

def table_support_css(details_background_colour):
    with open(os.path.join(os.path.dirname(os.path.realpath(__file__)), "hover-details.css")) as css_stream:
        return css_stream.read() % details_background_colour

def write_table_support_css(stream, details_background_colour):
    stream.write(table_support_css(details_background_colour))

def last_update_at_least_about_a_day_ago(filename):
    return ((not os.path.isfile(filename))
            or ((datetime.datetime.fromtimestamp(os.path.getmtime(filename))
                 + datetime.timedelta(hours=23, minutes=30))
                < datetime.datetime.now()))

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
