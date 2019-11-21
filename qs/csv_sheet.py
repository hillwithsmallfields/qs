#!/usr/bin/python3

import csv
import datetime
import os.path
import qsutils

def trim_if_float(val):
    return (("%.2F" % val)
            if type(val) is float
            else val)

class cvs_sheet:
    """A spreadsheet with headers."""

    def __init__(self, config, format_name=None):
        self.config = config
        self.header_row_number = 0
        self.format_name = format_name
        self.format = config['formats'][format_name] if format_name else None
        self.column_names = self.format['columns'] if self.format else {}
        self.rows = {}
        self.row_order = None
        self.row_cursor = None

    def __iter__(self):
        self.row_order = sorted(self.rows.keys())
        self.row_cursor = iter(self.row_order)
        return self

    def __next__(self):
        return self.rows[next(self.row_cursor)]

    def _unused_timestamp_from(self, base_date, base_time):
        base_timestamp = base_date+"T"+base_time
        return (self._unused_timestamp_from((datetime.datetime.strptime(base_timestamp,
                                                                       "%Y-%m-%dT%H:%M:%S")
                                            + datetime.timedelta(0,1)).isoformat())
                if base_timestamp in self.rows
                else base_timestamp)

    def get_cell(self, row, canonical_colum_name, default_value=None):
        """Get a cell value from a row, using its canonical column name."""
        return (get(row, self.column_names[canonical_colum_name], default_value)
                if canonical_colum_name in self.column_names
                else None)

    def set_cell(self, row, canonical_colum_name, value):
        """Set a cell value from in row, using its canonical column name.
        If that column is not defined in this format, do nothing."""
        if canonical_colum_name in self.column_names:
            row[self.column_names[canonical_colum_name]] = value

    def get_row_timestamp(self, row):
        return self.get_cell(row, 'date')+"T"+self.get_cell(row, 'time', self.default_time)

    def add_row(self, row):
        self.rows[this._unused_timestamp_from(self.get_cell(row, 'date'),
                                              self.get_cell(row, 'time', self.default_time))] = row

    def read(self, filename):
        with open(os.path.expanduser(os.path.expandvars(filename))) as infile:
            self.format_name, self.header_row_number = qsutils.deduce_stream_format(infile, config, args.verbose)
            for i in range(1, self.header_row_number):
                dummy = infile.readline()
            self.format = self.config['formats'][self.format_name]
            self.column_names = self.format['columns']
            self.default_time = (self.format['column_defaults'].get('time', "01:00:00")
                                 if 'column_defaults' in self.format
                                 else "01:00:00")
            self.rows = {this._unused_timestamp_from(self.get_cell(row0, 'date'),
                                                     self.get_cell(row0, 'time', self.default_time)):
                         {k:v for k,v in row0.iteritems() if k != ''}
                         for row0 in csv.DictReader(infile)}

    def write(self, filename):
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            colseq = self.format['column-sequence']
            writer = csv.DictWriter(outfile, colseq)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                # select only the columns required for this sheet, and
                # also trim out the unfortunately-represented floats
                writer.writerow({ k: trim_if_float(row[sk]) for sk in colseq})

# tests

import argparse
DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def main():
    """Tests for this module."""
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append',
                        help="""Extra config file (may be given multiple times).""")
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')
    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.load_config(args.verbose,
                                 DEFAULT_CONF if not args.no_default_config else None,
                                 *args.config)
    for filename in args.input_files:
        sheet = cvs_sheet(config)
        print("sheet is", sheet)
        print("iter is", iter(sheet))
        print("next value is", next(sheet))
        for row in iter(sheet):
            print(row)

if __name__ == "__main__":
    main()
