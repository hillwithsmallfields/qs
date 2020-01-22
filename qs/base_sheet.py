#!/usr/bin/python3

import csv
import datetime
import ordered_set
import os.path
import qsutils

class base_sheet:

    """To hold methods in common between csv_sheet and canonical_sheet."""

    def __init__(self, config):
        self.rows = {}
        self.config = config

    def timestamp_from(self, base_date, base_time=None):
        """Return a timestamp at the given date and time."""
        if isinstance(base_date, datetime.datetime):
            return base_date
        if isinstance(base_date, datetime.date):
            base_date = base_date.isoformat()
        if 'T' in base_date:    # allow base_date to be a whole timestamp
            timestamp = base_date
        else:
            base_time = (base_time or self.default_time)
        if isinstance(base_time, datetime.time):
            base_time = base_time.isoformat()
        timestamp = datetime.datetime.strptime(base_date+"T"+base_time,
                                               "%Y-%m-%dT%H:%M:%S")
        return timestamp

    def unused_timestamp_from(self, base_date, base_time=None):
        """Return a timestamp at the given date and time, that is not already
        used for a row. The soonest available time after the one specified is
        used in case of clashes, which will usually retain the order in which
        rows were added, even if only dates are given."""
        timestamp = self.timestamp_from(base_date, base_time)
        while timestamp in self.rows:
            timestamp += datetime.timedelta(0,1)
        return timestamp

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
                row = self.rows[timestamp]
                writer.writerow({sk: qsutils.trim_if_float(row.get(sk, None)) for sk in colseq})
