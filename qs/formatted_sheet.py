#!/usr/bin/python3

import canonical_sheet
import csv_sheet

class formatted_sheet(csv_sheet):

    def __init__(self,
                 config,
                 format_name,
                 canonical_input):
        super().__init__()
        for row_timestamp, row in canonical_input.items():
            self.rows[row_timestamp] = canonical_input.row_from_canonical(self.format, row)
