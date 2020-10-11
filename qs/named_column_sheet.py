#!/usr/bin/python3

import csv
import json
import os

import base_sheet
import qsutils

def subtracted_row(row, other_row):
    return {colname: row.get(colname, 0) - other_row.get(colname, 0)} 

def thresholded_row(row, other_row):
    return {colname: row[colname] if colname in row and abs(row[colname] < threshold)}
        
class named_column_sheet(base_sheet.base_sheet):

    def __init__(self, config, column_names):
        super().__init__(config)
        self.column_names = column_names

    def subtract_cells(self, other):
        """Return the cell-by-cell subtraction of another sheet from this one."""
        result = named_column_sheet(self.config, self.column_names)
        result.rows = {date: subtracted_row(row, other.rows.get(date, {}))}
        return result

    def abs_threshold(self, threshold):
        """Return a sheet like this but with any entries smaller than a given threshold omitted."""
        result = named_column_sheet(self.config, self.column_names)
        result.rows = {date: thresholded_row(row, threshold)}
        return result

    def occupied(self):
        """Return a sheet like this but with only the columns that are in use."""
        result = named_column_sheet(self.config, self.column_names)
        result.rows = self.rows
        column_names_seen = set()
        for row in self.rows.values():
            column_names_seen |= [k for k, v in row.items() if v not in (None, "", 0, 0.0)]
        result.column_names = [n for n in self.column_names if n in column_names_seen]
        return result
        
    def write_csv(self, filename):
        """Write a named-column sheet to a CSV file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            writer = csv.writer(outfile)
            writer.writerow(['Date'] + self.column_names)
            for date in sorted(self.rows):
                row_data = self.rows[date]
                writer.writerow([date] + [row_data.get(n, '') for n in self.column_names])

    def sparse_row(self, row):
        """Return a dictionary containing the occupied entries in a row."""
        return {str(k): qsutils.trim_if_float(v) for k, v in row.items() if k in self.column_names and v not in (None, "", 0, 0.0)}

    def sparse_contents(self):
        return {str(date): self.sparse_row(self.rows[date]) for date in sorted(self.rows.keys())}

    def write_json(self, filename):
        """Write a named-column sheet to a CSV file."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
            json.dump(self.sparse_contents(), outfile, indent=2)
            outfile.write("\n")
