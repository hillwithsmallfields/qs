import base_sheet
import csv
import os.path

class trace_sheet(base_sheet.base_sheet):

    """A spreadsheet for debugging sheet handling activities.

    Its rows will normally be a copy of another sheet's rows, added as
    they are added, but there will be at least one extra column which
    will store debugging output from the processing of that row.

    If the sheet file already exists, its existing contents will be
    read and added to, and the resulting sheet written back to that
    name, so you can use the same sheet for debugging several stages
    of an operation.
    """

    def __init__(self, config, filename):
        super().__init__(config)
        self.filename = filename
        self.default_time = "01:02:03"
        if os.path.exists(filename):
            self.read(filename)

    def read(self, filename):
        with open(os.path.expanduser(os.path.expandvars(filename))) as infile:
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

    def get_cell(self, row, column_name, default_value=None):
        """Get a cell value from a row, using its column name."""
        return row.get(column_name, default_value)

    def add_row(self, row, comment_type, comment):
        row = row.copy()
        row[comment_type] = comment
        ts = self.unused_timestamp_from(row['date'], row.get('time'))
        row['timestamp'] = ts
        self.rows[ts] = row

    def write_csv(self, filename=None, suppress_timestamp=False):
        self.write_all_columns(filename or self.filename)
