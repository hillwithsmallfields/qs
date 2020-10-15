#!/usr/bin/python3

import csv
import json
import os

import base_sheet
import qsutils
        
class named_column_sheet(base_sheet.base_sheet):

    def __init__(self, config, column_names):
        super().__init__(config)
        self.column_names = column_names

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
        
    def write_csv(self, filename):
        """Write a named-column sheet to a CSV file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            writer = csv.writer(outfile)
            writer.writerow(['Date'] + self.column_names_list())
            for date in sorted(self.rows):
                row_data = self.rows[date]
                writer.writerow([date] + [row_data.get(n, '') for n in self.column_names_list()])

    def sparse_row(self, row):
        """Return a dictionary containing the occupied entries in a row."""
        return {str(k): qsutils.trim_if_float(v)
                for k, v in row.items()
                if k in self.column_names_list() and v not in (None, "", 0, 0.0)}

    def sparse_contents(self):
        return {str(date): self.sparse_row(self.rows[date]) for date in sorted(self.rows.keys())}

    def write_json(self, filename):
        """Write a named-column sheet to a CSV file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            json.dump(self.sparse_contents(), outfile, indent=2)
            outfile.write("\n")
