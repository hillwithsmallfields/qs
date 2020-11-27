import copy

import qsutils

def filtered_by_date(sheet, compare_chars, use_new):
    result = copy.copy(sheet)
    result.rows = {}
    dates = sorted(sheet.rows.keys())
    current_timestamp = dates[0]
    current_timestamp_text = current_timestamp.isoformat()
    current_date = current_timestamp_text[:compare_chars]
    for timestamp in dates:
        timestamp_text = timestamp.isoformat()
        date = timestamp_text[:compare_chars]
        if date != current_date:
            if use_new:
                result.rows[timestamp] = sheet.rows[timestamp]
            else:
                result.rows[current_timestamp] = sheet.rows[current_timestamp]
        current_timestamp = timestamp
        current_timestamp_text = timestamp_text
        current_date = date
    return result
