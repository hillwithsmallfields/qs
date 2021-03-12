import copy
import datetime

import named_column_sheet
import qsutils

def filtered_by_date(sheet, period, use_new):
    """Make a sheet containing the first or last entry in each year or month of the original.
    Uses a given number of characters of the dates to determine
    whether two dates are the same."""
    result = copy.copy(sheet)
    result.rows = {}
    dates = sorted(sheet.rows.keys())
    current_timestamp = dates[0]
    current_date = period(current_timestamp)
    for timestamp in dates:
        date = period(timestamp)
        if date != current_date:
            if use_new:
                result.rows[timestamp] = sheet.rows[timestamp]
            else:
                result.rows[current_timestamp] = sheet.rows[current_timestamp]
        current_timestamp = timestamp
        current_date = date
    return result

def join_by_dates(sheet_a, sheet_b, period):
    """Make a sheet containing data from two sheets.

    Uses a given function on the dates to determine whether two dates
    are the same, taking the dates from the keys of the rows
    dictionary of each sheet.

    This assumes that there is only one entry for each date in each of
    the input sheets.  The results may not be meaningful if this is
    not the case.

    """
    result = copy.copy(sheet_a)
    result.rows = {}
    for column in sheet_b.column_names:
        if column not in result.column_names:
            result.column_names.append(column)
    date_columns = {}
    for column in ('Time', 'Date', 'Timestamp'):
        found = (column
                 if column in result.column_names
                 else (column.lower()
                       if column.lower() in result.column_names
                       else None))
        if found:
            date_columns[column.lower()] = found
            result.column_names.remove(found)
            result.column_names.insert(0, found)
    by_a_keys = {period(k): v for k, v in sheet_a.rows.items()}
    by_b_keys = {period(k): v for k, v in sheet_b.rows.items()}
    for key in sorted(set([ak for ak in by_a_keys.keys()] + [bk for bk in by_b_keys.keys()])):
        row = {}
        if key in by_a_keys:
            row.update(by_a_keys[key])
        if key in by_b_keys:
            row.update(by_b_keys[key])
        if 'timestamp' in date_columns:
            row[date_columns['timestamp']] = key
        if 'date' in date_columns:
            row[date_columns['date']] = key.date()
        if 'time' in date_columns:
            row[date_columns['time']] = key.time()
        result.rows[key] = row
    return result

def count_by_dates(sheet, period):
    """Make a sheet containing the count of transactions in each period in the original sheet."""
    counts = {}
    for timestamp in sheet.rows:
        start = period(timestamp)
        counts[start] = counts.get(start, 0) + 1
    return named_column_sheet.named_column_sheet(
        sheet.config,
        ['timestamp', 'count'],
        rows = {ts: {'timestamp': ts,
                     'count': counts[ts]}
                for ts in counts})

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

