import copy
import datetime

import qsutils

def filtered_by_date(sheet, compare_chars, use_new):
    """Make a sheet containing the first or last entry in each year or month of the original.
    Uses a given number of characters of the dates to determine
    whether two dates are the same."""
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

def join_by_dates(sheet_a, sheet_b, compare_chars):
    """Make a sheet containing data from two sheets.
    Uses a given number of characters of the dates to determine
    whether two dates are the same.
    This assumes that there is only one entry for each date in each of
    the input sheets.  The results may not be meaningful if this is
    not the case."""
    the_first = "-01-01T01:02:03"[10-compare_chars:]
    result = copy.copy(sheet_a)
    result.rows = {}
    for column in sheet_b.column_names:
        if column not in result.column_names:
            result.column_names.append(column)
    if 'date' not in result.column_names:
        result.column_names = ['date'] + result.column_names
    # if 'timestamp' not in result.column_names:
    #     result.column_names = ['timestamp'] + result.column_names
    by_a_keys = {k.isoformat()[:compare_chars]: v for k, v in sheet_a.rows.items()}
    by_b_keys = {k.isoformat()[:compare_chars]: v for k, v in sheet_b.rows.items()}
    for key in sorted(set([ak for ak in by_a_keys.keys()] + [bk for bk in by_b_keys.keys()])):
        row = {}
        if key in by_a_keys:
            row.update(by_a_keys[key])
        if key in by_b_keys:
            row.update(by_b_keys[key])
        date_string = key + the_first
        # row['timestamp'] = datetime.datetime.fromisoformat(date_string)
        row['date'] = date_string[:10]
        result.rows[date_string] = row
    return result

def annotate_by_timestamp(sheet, reference, annotation_columns):
    result = copy.copy(sheet)
    result.rows = {}
    result.colseq = sheet.colseq + annotation_columns
    for timestamp, row in sheet.rows.items():
        new_row = row.copy()
        result.rows[timestamp] = new_row
        if timestamp in reference.rows:
            rr = reference.rows[timestamp]
            for ac in annotation_columns:
                if ac in rr:
                    new_row[ac] = rr[ac]
    return result

