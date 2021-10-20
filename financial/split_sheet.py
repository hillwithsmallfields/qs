#!/usr/bin/python3

import csv

import itemized_amount

class split_sheet():

    """A derived spreadsheet that combines rows of its original that are the same in some characteristic.
    Not based on base_sheet, as that assumes datetimes are used."""

    def __init__(self, original, splitter, key_column, result_column):
        self.rows = {}
        self.key = key_column
        self.columns = [key_column, result_column]
        for row in original.rows.values():
            partition = splitter(row)
            if partition in self.rows:
                self.rows[partition][result_column] += row
            else:
                self.rows[partition] = {
                    key_column: partition,
                    result_column: itemized_amount.itemized_amount(row)}

    def sample(self, stream, label, sample_count=3):
        """Print a sample of this sheet to a stream."""
        stream.write(label + " --- sample of " + str(type(self)) + ":\n")
        stream.write("columns are [" + ", ".join(self.columns) + "]\n")
        for row_key in sorted(self.rows.keys())[::len(self.rows)//sample_count]:
            stream.write("  " + repr(row_key) + ":\n")
            row = self.rows[row_key]
            for col_key in sorted(row.keys()):
                stream.write("    " + repr(col_key) + ": " + repr(row[col_key]) + "\n")

    def write_csv(self, filename):
        with open(filename, 'w') as outstream:
            writer = csv.DictWriter(outstream, self.columns)
            writer.writeheader()
            for row_key in sorted(self.rows.keys()):
                 row = self.rows[row_key]
                 writer.writerow(row)

    def write_html_table(self, stream,
                         css_class=None,
                         hover_details=False,
                         col_extra_data=None,
                         start_date=None, end_date=None,
                         with_time=False,
                         summarize=True,
                         colnames=None):
        stream.write('<table')
        if css_class:
            stream.write(' class="%s"' % css_class)
        stream.write('>\n')

        write_headings(stream, colnames)

        write_extra_data(stream, col_extra_data, "Threshold", colnames)

        for partition in sorted(self.rows.keys()):
            row = self.rows[partition]
            stream.write('<tr>')
            stream.write('<th>%s</th>' % str(partition))
            stream.write('<td>%s</td>' % row)
            stream.write('</tr>\n')

        stream.write('</table>\n')
