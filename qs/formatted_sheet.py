#!/usr/bin/python3

import canonical_sheet
import csv_sheet

class formatted_sheet(csv_sheet.csv_sheet):

    def __init__(self,
                 config,
                 format_name,
                 canonical_input):
        super().__init__(config, format_name=format_name)
        print("formatted_sheet format_name", format_name)
        print("formatted_sheet self.format", self.format)
        print("formatted_sheet self.format['column-sequence']", self.format['column-sequence'])
        print("formatted_sheet self.column_names", self.column_names)
        reverse_equivalents = config.get('reverse-equivalents')
        for row in canonical_input:
            self.rows[row['timestamp']] = canonical_input.row_from_canonical(self.format, row, reverse_equivalents)
