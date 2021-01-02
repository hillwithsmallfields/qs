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
        self.default_time = "00:00:00"

    def __len__(self):
        return len(self.rows)

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

    def column_names_list():
        """Return the column names for this sheet."""
        colseq = ordered_set.OrderedSet()
        for row in self.rows.values():
            colseq |= row.keys()
        if 'date' in colseq:
            colseq.remove('date')
            colseq = ['date'] + [col for col in colseq]
        return colseq

    def column_average(self, colname, absolute):
        """Return the average value of the named column."""
        count = 0
        total = 0
        for row in self.rows.values():
            if colname in row:
                total += abs(row[colname]) if absolute else row[colname]
                count += 1
        return total / count if count > 0 else 0

    def add_row(self, row):
        """Add a copy of a row to this sheet, adjusting its timestamp
        to avoid clashing with existing rows."""
        row = row.copy()
        orig_account = row.get('account')
        account = orig_account
        name_table = self.config.get('reverse-equivalents')
        if orig_account and name_table and orig_account in name_table:
            row['account'] = name_table[orig_account]
        timestamp = self.unused_timestamp_from(row['timestamp'])
        row['timestamp'] = timestamp
        if 'time' in row:
            row['time'] = timestamp.time()
        if 'date' in row:
            row['date'] = timestamp.date()
        self.rows[timestamp] = row

    def write_all_columns(self, filename):
        """Write a spreadsheet.
        The column list is generated from the row contents,
        with the columns in the order they are first seen."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            colseq = self.column_names_list()
            writer = csv.DictWriter(outfile, colseq)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                writer.writerow({sk: qsutils.tidy_for_output(row.get(sk, "")) for sk in colseq})

    def write_csv(self, filename, suppress_timestamp=False):
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

    def write_html_table(self, stream,
                         _class=None,
                         hover_details=False,
                         start_date=None, end_date=None):
        """Write a canonical spreadsheet as HTML."""
        dates = sorted(self.rows.keys())
        colnames = self.column_names_list()
        start = 0
        if start_date:
            for i in range(len(self.rows)):
                if self.rows[dates[i]]['timestamp'] >= start_date:
                    start = i
                    break
        end = len(dates)
        if end_date:
            for i in range(start, end):
                if self.rows[dates[i]]['timestamp'] > end_date:
                    end = i
                    break
        stream.write('<table border=1')
        if _class:
            stream.write(' class="%s"', _class)
        stream.write('>\n  <tr>\n')
        for colname in colnames:
            stream.write('    <th>%s</th>\n' % colname)
        stream.write('  </tr>\n')
        for i in range(start, end):
            date = dates[i]
            row = self.rows[date]
            stream.write('  <tr>\n')
            stream.write('    <th class="date">%s</th>\n' % date)
            for colname in colnames:
                stream.write('    <td class="%s"><span class="overview">%s' % (colname.replace(' ', '_'), qsutils.tidy_for_output(row.get(colname, ""))))
                if hover_details:
                    stream.write('<span class="details">Details to go here</span>')
                stream.write('</span></td>\n')
            stream.write('  </tr>\n')
        stream.write('</table>\n')
