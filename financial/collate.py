#!/usr/bin/env python3

import argparse
from collections import defaultdict
import datetime
import functools
import operator

import financial.finutils

def collate(incoming, key, period, column_mapping=None):

    """Returns a collation of a table.

    The input is a table as returned by csv.DictReader, with a 'Date'
    column, with a row for each transaction.

    The output is a table with a row for each period (day, month,
    year, weekday), with a column for each value of a selected field
    (such as 'category' or 'payee'), and the cells are lists of the
    original rows matching that period and that field value.

    A map may be given to map multiple input keys to fewer output keys.
    """

    period_fn = {
        'day': lambda isodate: isodate,
        'month': lambda isodate: isodate[:7],
        'year': lambda isodate: isodate[:4],
        'weekday': lambda isodate: str(datetime.date.fromisoformat(isodate).weekday()),
    }[period]
    result = defaultdict(lambda: defaultdict(list))
    for row in incoming:
        if key not in row:
            print("    row", row, "is missing key", key)
            continue
        result[period_fn(row['Date'])][column_mapping.get(row[key], row[key])
                                       if column_mapping
                                       else row[key]].append(row)
    return financial.finutils.with_key_as_column(result, 'Date')

def ize(table, izefn):
    """Returns a table containing the result of applying a function to each cell of the input.

    The input is a list of dictionaries, similar to the result of
    csv.DictReader, but with each cell being a list of the rows as
    produced by csv.DictReader.

    The output is a list of dictionaries, suitable for csv.DictWriter.

    May be used to summarize or textualize the table.
    """
    return [
        {
            column: izefn(cell)
            for column, cell in row.items()
            if column != 'Date'
        } | {'Date': row['Date']}
        for row in table
    ]

def summarize(table):
    """Return a table containing the summary of each cell of the input."""
    return ize(table,
               lambda cell: "{:.2f}".format(functools.reduce(operator.add,
                                                             (float(item['amount'])
                                                              for item in cell))))

def textualize(table):
    """Return a table containing a textual representation of each cell of the input."""
    return ize(table,
               lambda cell: "; ".join(("{} {} {}".format(item['Date'],
                                                         item['payee'],
                                                         item['amount'])
                                       for item in cell)))

def collate_in_files(incoming, key, period, summary, output,
                     starting=None, ending=None):
    """Collates a table from a file, with the output to files.

    The 'summary' file is a table with the total transactions for each
    period and classification.

    The 'output' file has all the transactions for each period and
    classification, in a short textual form.

    """
    result = collate(financial.finutils.read_csv(incoming, starting, ending), key, period)
    headings = financial.finutils.bring_to_front(sorted(financial.finutils.headings(result)), {'Date'})
    if summary:
        financial.finutils.write_csv(summarize(result), headings, summary, lambda r: r['Date'])
    if output:
        financial.finutils.write_csv(textualize(result), headings, output, lambda r: r['Date'])
