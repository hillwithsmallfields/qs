#!/usr/bin/python3

import csv
import json
import numbers
import os

import base_sheet
import canonical_sheet
import qsutils

class named_column_sheet(base_sheet.base_sheet):

    def __init__(self, config, column_names, rows={}):
        super().__init__(config)
        if rows:
            print("making named_column_sheet with columns", column_names, "and sample row", rows[sorted(rows.keys())[len(rows)//2]])
        self.column_names = column_names
        self.rows = rows

    def column_names_list(self):
        return self.column_names

    def subtract_cells(self, other):
        """Return the cell-by-cell subtraction of another sheet from this one."""
        column_names = self.column_names_list()
        result = named_column_sheet(self.config, column_names)
        result.rows = {date: qsutils.subtracted_row(row,
                                            other.rows.get(date, {}),
                                            column_names)
                       for date, row in self.rows.items()}
        return result

    def proportions(self):
        """Return a sheet like this but with the values in each row given as
        the original value divided by the total for the row."""
        result = named_column_sheet(self.config, self.column_names_list())
        result.rows = {}
        for ts, row in self.rows.items():
            total = sum(row.values())
            result.rows[ts] = {colname: cell/total for colname, cell in row.items()}
        return result

    def abs_threshold(self, threshold):
        """Return a sheet like this but with any entries smaller than a given threshold omitted."""
        result = named_column_sheet(self.config, self.column_names_list())
        result.rows = {date: qsutils.thresholded_row(row, threshold)
                       for date, row in self.rows.items()}
        return result

    def occupied_columns(self):
        """Return a sheet like this but with only the columns that are in use."""
        result = named_column_sheet(self.config, self.column_names_list())
        result.rows = self.rows
        column_names_seen = set()
        for row in self.rows.values():
            column_names_seen |= set([k for k, v in row.items() if v not in (None, "", 0, 0.0)])
        result.column_names = [n for n in self.column_names_list() if n in column_names_seen]
        return result

    def between_dates(self, begin_date, end_date):
        """Return a sheet like this but with only the rows in a given date range.
        False values for either end of the range mean no limit in that direction."""
        if begin_date:
            begin_date = qsutils.as_date(begin_date)
        if end_date:
            end_date = qsutils.as_date(end_date)
        result = named_column_sheet(self.config, self.column_names_list())
        result.rows = {timestamp: row
                       for timestamp, row in self.rows.items()
                       if (((not begin_date)
                            or timestamp >= begin_date)
                           and ((not end_date)
                                or timestamp <= end_date))}
        return result

    def annotate_matches(self, reference):
        """Make a named column sheet with matches to a reference sheet noted."""
        result = named_column_sheet(self.config, self.column_names_list())
        for timestamp, row in self.rows.items():
            annotated_row = {}
            for k, v in row.items():
                if isinstance(v, numbers.Number) and abs(v) > 0:
                    possibilities = reference.find_amount(v, timestamp, 7, k)
                    originals = ["%s: %s" % (r['payee'], r['timestamp'].date())
                                 for r in possibilities]
                    annotated_row[k] = qsutils.tidy_for_output(v) + ((":" + ";".join(originals))
                                                                   if originals else "")
            result.rows[timestamp] = annotated_row
        return result

    def make_update_sheet(self, reference):
        """Make a canonical sheet from matches to a reference sheet."""
        result = canonical_sheet.canonical_sheet(self.config)
        for timestamp, row in self.rows.items():
            for k, v in row.items():
                if isinstance(v, numbers.Number) and abs(v) > 0:
                    for found_row in reference.find_amount(v, timestamp, 7, k):
                        new_row = found_row.copy()
                        result.rows[new_row['timestamp']] = new_row
        return result

    def averages(self):
        totals = {}
        for row in self.rows.values():
            for name, value in row.items():
                print(".", end="")
                totals[name] = value + totals.get(name, 0)
            print(":")
        count = len(self.rows)
        return {name: value/count for name, value in totals.items()}

    def write_csv(self, filename, suppress_timestamp=False, show_averages=False):
        """Write a named-column sheet to a CSV file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        # print("named_column_sheet.write_csv", filename, "suppress_timestamp", suppress_timestamp, self)
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            writer = csv.writer(outfile)
            writer.writerow(([] if suppress_timestamp else ['Date']) + [n for n in self.column_names if n != 'Date'])
            if show_averages:
                avs = self.averages()
                writer.writerow(['Averages'] + [qsutils.tidy_for_output(avs.get(col, ""))
                                                for col in self.column_names])
            for date in sorted(self.rows):
                row_data = self.rows[date]
                writer.writerow(([]
                                 if suppress_timestamp
                                 else [date]) + [qsutils.tidy_for_output(row_data.get(n, ''))
                                                 for n in self.column_names
                                                 if n != 'Date'])

    def sparse_row(self, row):
        """Return a dictionary containing the occupied entries in a row."""
        return {str(k): qsutils.tidy_for_output(v)
                for k, v in row.items()
                if k in self.column_names_list() and v not in (None, "", 0, 0.0)}

    def sparse_contents(self):
        return {str(date): self.sparse_row(self.rows[date]) for date in sorted(self.rows.keys())}

    def write_json(self, filename):
        """Write a named-column sheet to a JSON file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            json.dump(self.sparse_contents(), outfile, indent=2)
            outfile.write("\n")
