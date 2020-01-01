#!/usr/bin/python3

import canonical_sheet
import csv_sheet

class formatted_sheet(csv_sheet.csv_sheet):

    def __init__(self,
                 config,
                 format_name,
                 canonical_input):
        super().__init__(config, format_name=format_name)
        for row in canonical_input:
            self.rows[row['timestamp']] = canonical_input.row_from_canonical(self.format, row)
