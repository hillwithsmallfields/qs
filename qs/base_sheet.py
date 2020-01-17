#!/usr/bin/python3

import datetime

class base_sheet:

    """To hold methods in common between csv_sheet and canonical_sheet."""

    def __init__(self, config):
        self.rows = {}
        self.config = config

    def unused_timestamp_from(self, base_date, base_time=None):
        """Return a timestamp at the given date and time, that is not already
        used for a row. The soonest available time after the one specified is
        used in case of clashes, which will usually retain the order in which
        rows were added, even if only dates are given."""
        print("  given", base_date, base_time)
        if 'T' in base_date:    # allow base_date to be a whole timestamp
            timestamp = base_date
        else:
            base_time = (base_time or self.default_time)
            timestamp = datetime.datetime.strptime(base_date+"T"+base_time,
                                                   "%Y-%m-%dT%H:%M:%S")
        print("  made", timestamp)
        while timestamp in self.rows:
            timestamp += datetime.timedelta(0,1)
            print("    bumped to", timestamp)
        return timestamp
