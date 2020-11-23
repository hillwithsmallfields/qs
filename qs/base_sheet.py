#!/usr/bin/python3

import csv
import datetime
import ordered_set
import os.path
import qsutils

class base_sheet:

    """To hold methods in common between csv_sheet and canonical_sheet."""

    def __init__(self, config, rows=None):
        self.rows = rows or {}
        self.config = config
        self._hide_in_csv = True # don't show these if they get incorporated into row data

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
        """Write a spreadsheet.
        The column list is generated from the row contents,
        with the columns in the order they are first seen."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            colseq = ordered_set.OrderedSet()
            for row in self.rows.values():
                colseq |= row
            if 'date' in colseq:
                colseq.remove('date')
                colseq = ['date'] + [col for col in colseq]
            writer = csv.DictWriter(outfile, colseq)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                writer.writerow({sk: qsutils.trim_if_float(row.get(sk, "")) for sk in colseq})

    def write_csv(self, filename):
        """Write a spreadsheet.
        This will normally be overridden in subclasses."""
        self.write_all_columns(filename)

    def chart(self, title, filename, fields):
        base_file = os.path.splitext(filename)[0]
        bitmap_filename = base_file + ".png"
        data_filename = base_file + ".dat"
        print("chart given fields", fields)
        if fields[0] != 'date':
            fields = ['date'] + fields
            print("fields is now", fields)
        with open(data_filename, 'w') as datafile:
            datawriter = csv.writer(datafile)
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                for field in fields:
                    if field not in row:
                        print("field", field, "not in", row)
                datawriter.writerow([row.get(field, "")
                                     for field in fields])
        with open(filename, 'w') as plot:
            plot.write('set title "%s"\n' % title)
            # set terminal png size 2560,1920
            plot.write('set output "%s"\n' % bitmap_filename)
            plot.write('set timefmt x "%Y-%m-%d"\n')
            plot.write('set xdata time\n')
            plot.write('set format x "%Y-%m"\n')
            # # set xrange [ "80.74":"104.42" ]
            # set xtics rotate by 45 border offset 0,.5 out nomirror 2419200
            # set ylabel "Kilograms"
            # set y2label "Kg change in week"
            plot.write('set ytics nomirror\n')
            plot.write('set y2tics\n')
            plot.write('set grid xtics\n')
            plot.write('set datafile separator ","\n')
            # plot "/tmp/plot-weight.dat" using 1:7 with line axes x1y1 lc 7 title "Weight", "/tmp/plot-weight.dat" using 1:12 with line axes x1y1 lc 1 title "Average (week)", "/tmp/plot-weight.dat" using 1:14 with line axes x1y2 lc 3 title "Change (average) in week"
            plot.write("plot " + ", ".join([" %s using %d title \"%s\"" % (data_filename, index+1, fieldname)
                                            for index, fieldname in enumerate(fields[1:])]) + "\n")
