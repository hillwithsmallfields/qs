#!/usr/bin/python3

import base_sheet
import csv
import os.path
import qsutils

class UnknownCSVFormat(BaseException):
    pass

    def __init__(self, infile):
        self.infile = infile

class csv_sheet(base_sheet.base_sheet):
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
        super().__init__(config)
        self.default_time = default_time
        self.verbose = verbose
        self.header_row_number = 0
        self.filename = input_filename
        self.origin_files = []
        print("csv_sheet.__init__ format_name", format_name, "input", input_filename)
        if input_filename:
            if not self.read(input_filename):
                print("Could not construct csv_sheet from", input_filename)
        else:
            self.format_name = format_name
            self.format = (config['formats'][format_name]
                           if format_name in config['formats']
                           else None)
            print("csv_sheet.__init__ got format", self.format)
            self.column_names = self.format['columns'] if self.format else {}
        self.currency_conversion = self.format.get('currency_conversion', None)
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
                + " format with " + str(len(self.rows)) + " rows"
                + ((" and columns ["
                    + ", ".join(self.format['column-sequence'])
                    + "]") if 'column-sequence' in self.format else "")
                + ">")

    def get_cell(self, row, canonical_column_name, default_value=None):
        """Get a cell value from a row, using its canonical column name."""
        return (row.get(self.column_names[canonical_column_name], default_value)
                if canonical_column_name in self.column_names
                else default_value)

    def get_numeric_cell(self, row, canonical_column_name, default_value=None):
        """Get a numeric cell value from a row, using its canonical column name."""
        raw_value = (row.get(self.column_names[canonical_column_name], default_value)
                     if canonical_column_name in self.column_names
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
            if self.format_name is None:
                if not self.verbose:
                    self.format_name, self.header_row_number = qsutils.deduce_stream_format(infile, self.config, verbose=True)
                raise UnknownCSVFormat(infile)
            print("Detected", self.format_name, "spreadsheet in", filename)
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
            # We can't construct this with a dictionary comprehension,
            # because unused_timestamp_from needs to see the entries
            # so far as the dictionary is filled in, and the
            # comprehension mechanism constructs all the values then
            # puts them all in place:
            for row0 in csv.DictReader(infile):
                canonized = {k:v for k,v in row0.items() if k != ''}
                unique_ts = self.unused_timestamp_from(self.get_cell(row0, 'date'),
                                                       self.get_cell(row0, 'time', self.default_time))
                canonized['timestamp'] = unique_ts
                self.rows[unique_ts] = canonized
        for row in self.rows.values():
            row.update(sheet_marker)
        self.origin_files.append(filename)
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
                writer.writerow({sk: qsutils.tidy_for_output(row.get(sk, "")) for sk in colseq})

    def write_debug(self, filename):
        """Write a account to a file, for debugging."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            outfile.write(str(self))

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
