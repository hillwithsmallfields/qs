#!/usr/bin/python3

import csv
import datetime
import os.path
import qsutils

def trim_if_float(val):
    return (("%.2F" % val)
            if type(val) is float
            else val)

class csv_sheet:
    """A spreadsheet with headers and column name translation.

    The name translation allows the caller to use a standard column naming
    scheme even when the spreadsheets are read from sources with a mixture
    of naming schemes (such as statements from multiple banks).

    The rows are assumed to be timestamped, and can be iterated over,
    and output, in time order.
    """

    def __init__(self,
                 config,
                 format_name=None,
                 input_filename=None,
                 default_time="01:00:00"):
        self.config = config
        self.default_time = default_time
        self.rows = {}
        self.header_row_number = 0
        if input_filename:
            self.read(input_filename)
        else:
            self.format_name = format_name
            self.format = config['formats'][format_name] if format_name else None
            self.column_names = self.format['columns'] if self.format else {}
        self.row_order = None
        self.row_cursor = 0

    def __iter__(self):
        self.row_order = sorted(self.rows.keys())
        self.row_cursor = -1    # because we pre-increment it
        return self

    def __next__(self):
        self.row_cursor += 1
        if self.row_cursor >= len(self.row_order):
            raise StopIteration
        return self.rows[self.row_order[self.row_cursor]]

    def __str__(self):
        return ("<" + self.format_name + " spreadsheet with "
                + str(len(self.rows)) + " rows>")

    def __repr__(self):
        return ("<spreadsheet in " + self.format_name
                + " format with " + str(length(self.rows)) + " rows"
                ((" and columns ["
                  + ", ".join(self.format['column-sequence'])
                  + "]") if 'column-sequence' in self.format else "")
                + ">")

    def unused_timestamp_from(self, base_date, base_time):
        """Return a timestamp at the given date and time, that is not already
        used for a row. The soonest available time after the one specified is
        used in case of clashes, which will usually retain the order in which
        rows were added, even if only dates are given."""
        base_timestamp = base_date+"T"+(base_time or self.default_time)
        return (self.unused_timestamp_from((datetime.datetime.strptime(base_timestamp,
                                                                       "%Y-%m-%dT%H:%M:%S")
                                            + datetime.timedelta(0,1)).isoformat())
                if base_timestamp in self.rows
                else base_timestamp)

    def get_cell(self, row, canonical_colum_name, default_value=None):
        """Get a cell value from a row, using its canonical column name."""
        return (row.get(self.column_names[canonical_colum_name], default_value)
                if canonical_colum_name in self.column_names
                else default_value)

    def set_cell(self, row, canonical_column_name, value):
        """Set a cell value from in row, using its canonical column name.
        If that column is not defined in this format, do nothing."""
        if canonical_column_name in self.column_names:
            row[self.column_names[canonical_column_name]] = value

    def get_row_timestamp(self, row):
        return self.get_cell(row, 'date')+"T"+self.get_cell(row, 'time', self.default_time)

    def add_row(self, row):
        self.rows[self.unused_timestamp_from(self.get_cell(row, 'date'),
                                             self.get_cell(row, 'time', self.default_time))] = row

    def read(self, filename):
        """Read a spreadsheet, deducing the type.
        A collection of header lines is scanned to find the type."""
        with open(os.path.expanduser(os.path.expandvars(filename))) as infile:
            self.format_name, self.header_row_number = qsutils.deduce_stream_format(infile, self.config)
            for i in range(1, self.header_row_number):
                _ = infile.readline()
            self.format = self.config['formats'][self.format_name]
            self.column_names = self.format['columns']
            self.default_time = (self.format['column_defaults'].get('time', "01:00:00")
                                 if 'column_defaults' in self.format
                                 else "01:00:00")
            self.rows = {self.unused_timestamp_from(self.get_cell(row0, 'date'),
                                                    self.get_cell(row0, 'time', self.default_time)):
                         {k:v for k,v in row0.items() if k != ''}
                         for row0 in csv.DictReader(infile)}

    def write(self, filename):
        """Write a spreadsheet in a given format.
        Any columns not used by that format are ignored."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            colseq = self.format['column-sequence']
            writer = csv.DictWriter(outfile, colseq)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                # select only the columns required for this sheet, and
                # also round the unfortunately-represented floats
                writer.writerow({sk: trim_if_float(row[sk]) for sk in colseq})

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
        sheet = csv_sheet(config, input_filename=filename)
        print("sheet from", filename, "is", sheet)
        for row in iter(sheet):
            print(row)

if __name__ == "__main__":
    main()
