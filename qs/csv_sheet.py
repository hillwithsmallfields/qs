#!/usr/bin/python3

import csv
import datetime
import ordered_set
import os.path
import qsutils

class csv_sheet:
    """The contents of a CSV spreadsheet with headers.

    The 'format' describes the column names, and maps them to a
    standard set of names.  (The class canonical_csv uses the standard
    names.)

    The rows are assumed to be timestamped, and can be iterated over,
    and output, in time order.

    """

    def __init__(self,
                 config,
                 format_name=None,
                 input_filename=None,
                 default_time="01:00:00",
                 verbose=False):
        self.config = config
        self.default_time = default_time
        self.rows = {}
        self.verbose = verbose
        self.header_row_number = 0
        self.origin_files = []
        if input_filename:
            if not self.read(input_filename):
                print("Could not construct csv_sheet from", input_filename)
        else:
            self.format_name = format_name
            self.format = (config['formats'][format_name]
                           if format_name in config['formats']
                           else None)
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

    def __len__(self):
        return len(self.rows)

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

    def unused_timestamp_from(self, base_date, base_time=None):
        """Return a timestamp at the given date and time, that is not already
        used for a row. The soonest available time after the one specified is
        used in case of clashes, which will usually retain the order in which
        rows were added, even if only dates are given."""
        if 'T' in base_date:    # allow base_date to be a whole timestamp
            timestamp = base_date
        else:
            base_time = (base_time or self.default_time)
            timestamp = datetime.datetime.strptime(base_date+"T"+base_time,
                                                   "%Y-%m-%dT%H:%M:%S")
        while timestamp in self.rows:
            timestamp += datetime.timedelta(0,1)
        return timestamp

    def get_cell(self, row, canonical_column_name, default_value=None):
        """Get a cell value from a row, using its canonical column name."""
        return (row.get(self.column_names[canonical_column_name], default_value)
                if canonical_column_name in self.column_names
                else default_value)

    def get_numeric_cell(self, row, canonical_colum_name, default_value=None):
        """Get a numeric cell value from a row, using its canonical column name."""
        raw_value = (row.get(self.column_names[canonical_colum_name], default_value)
                     if canonical_colum_name in self.column_names
                     else default_value)
        try:
            return float(raw_value)
        except:
            return default_value

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
            self.format_name, self.header_row_number = qsutils.deduce_stream_format(infile, self.config, verbose=self.verbose)
            sheet_marker = {'sheet': self}
            for i in range(1, self.header_row_number):
                _ = infile.readline()
            if self.format_name not in self.config['formats']:
                print("Format name", self.format_name, "not known in", self.config['formats'].keys())
            self.format = (self.config['formats'][self.format_name]
                           if self.format_name in self.config['formats']
                           else self.config['formats']['Default'])
            self.column_names = self.format['columns']
            self.default_time = (self.format['column_defaults'].get('time', "01:00:00")
                                 if 'column_defaults' in self.format
                                 else "01:00:00")
            self.rows = {self.unused_timestamp_from(self.get_cell(row0, 'date'),
                                                    self.get_cell(row0, 'time', self.default_time)):
                         {k:v for k,v in row0.items() if k != ''}
                         for row0 in csv.DictReader(infile)}
        for row in self.rows.values():
            row.update(sheet_marker)
        self.origin_files.append(filename)
        print("csv_sheet.read origin files now", self.origin_files)
        return True

    def write_csv(self, filename):
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
                writer.writerow({sk: qsutils.trim_if_float(row.get(sk, None)) for sk in colseq})

    def write_all_columns(self, filename):
        """Write a spreadsheet in a given format.
        The column list is generated from the row contents,
        with the columns in the order they are first seen."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            colseq = ordered_set.OrderedSet()
            for row in self.rows.values():
                colseq |= row
            writer = csv.DictWriter(outfile, colseq)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                writer.writerow(self.rows[timestamp])

# tests

import argparse

def main():
    """Tests for this module."""
    parser = qsutils.program_argparser()
    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    for filename in args.input_files:
        print("reading test data from", filename)
        sheet = csv_sheet(config, input_filename=filename)
        print("sheet from", filename, "is", sheet)
        print("---- begin", len(sheet), sheet.format_name, "rows ----")
        for row in iter(sheet):
            print(row)
        print("---- end", sheet.format_name, "rows ----")

if __name__ == "__main__":
    main()
