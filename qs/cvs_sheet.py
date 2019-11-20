import csv
import datetime
import os.path
import qsutils

class cvs_sheet:
    """A spreadsheet with headers."""

    def __init__(self, config, format_name=None, column_names={}, rows):
        self.config = config
        self.header_row_number = 0
        self.format_name = None
        self.format = None
        self.column_names = column_names
        self.rows = rows

    def _unused_timestamp_from(self, base_date, base_time):
        base_timestamp = base_date+"T"+base_time
        return (self._unused_timestamp_from((datetime.datetime.strptime(base_timestamp,
                                                                       "%Y-%m-%dT%H:%M:%S")
                                            + datetime.timedelta(0,1)).isoformat())
                if base_timestamp in self.rows
                else base_timestamp)

    def get_cell(self, row, canonical_colum_name, default_value=None):
        return get(row, self.column_names[canonical_colum_name], default_value)

    def get_row_timestamp(self, row):
        # combine [column_names['date']] and [column_names['time']]
        pass

    def add_row(self, row):
        # use _unused_timestamp_from and get_row_timestamp
        pass

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
            writer = csv.DictWriter(outfile, self.format['column-sequence'])
            writer.writeheader()
            colseq = self.format['column-sequence']
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                writer.writerow({ k: (("%.2F" % v)
                                      if type(v) is float
                                      else v)
                                  for k, v in {sk: row[sk] for k in colseq}})
