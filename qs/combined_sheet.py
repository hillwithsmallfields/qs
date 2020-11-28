#!/usr/bin/python3

import base_sheet

class combined_sheet(base_sheet.base_sheet):

    """A spreadsheet made by merging several sheets by timestamp.
    This is not a join: each row remains a distinct row, with the timestamp
    being nudged along as necessary."""

    def __init__(self,
                 *input_sheets):
        super().__init__(*input_sheets[0].config,
                         verbose=*input_sheets[0].verbose)
        all_timestamps = {}
        for sheet in input_sheets:
            for row in iter(sheet):
                row_ts = sheet.get_row_timestamp(row)
                if row_ts in all_timestamps:
                    all_timestamps[row_ts].append(sheet)
                else:
                    all_timestamps[row_ts] = [sheet]
        for ts in sorted(all_timestamps.keys()):
            sheets_with_entry_at_ts = all_timestamps[ts]
            for sheet in sheets_with_entry_at_ts:
                self.rows[self.unused_timestamp_from(ts)] = sheet.rows[ts]
