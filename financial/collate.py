#!/usr/bin/env python3

import argparse
from collections import defaultdict
import datetime
import functools
import operator

import finutils

def collate(incoming, key, period):

    """Returns a collation of a table.

    The input is a table as returned by csv.DictReader, with a 'date'
    column, with a row for each transaction.

    The output is a table with a row for each period (day, month,
    year, weekday), with a column for each value of a selected field
    (such as 'category' or 'payee'), and the cells are lists of the
    original rows matching that period and that field value.
    """

    period_fn = {
        'day': lambda isodate: isodate,
        'month': lambda isodate: isodate[:7],
        'year': lambda isodate: isodate[:4],
        'weekday': lambda isodate: str(datetime.date.fromisoformat(isodate).weekday()),
    }[period]

    result = defaultdict(lambda: defaultdict(list))
    for row in incoming:
        result[period_fn(row['date'])][row[key]].append(row)
    return finutils.with_key_as_column(result, 'date')

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
            if column != 'date'
        } | {'date': row['date']}
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
               lambda cell: "; ".join(("{} {} {}".format(item['date'],
                                                         item['payee'],
                                                         item['amount'])
                                       for item in cell)))

def collate_in_files(incoming, key, period, summary, output):
    """Collates a table from a file, with the output to files.

    The 'summary' file is a table with the total transactions for each
    period and classification.

    The 'output' file has all the transactions for each period and
    classification, in a short textual form.

    """
    result = collate(finutils.read_csv(incoming), key, period)
    headings = finutils.bring_to_front(sorted(finutils.headings(result)), {'date'})
    if summary:
        finutils.write_csv(summarize(result), headings, summary, lambda r: r['date'])
    if output:
        finutils.write_csv(textualize(result), headings, output, lambda r: r['date'])

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--incoming", "-i", default=finutils.MAIN_ACCOUNTING_FILE)
    parser.add_argument("--key", "-k", default='category')
    parser.add_argument("--period", "-p", default='month')
    parser.add_argument("--output", "-o")
    parser.add_argument("--summary", "-s")
    return vars(parser.parse_args())

if __name__ == "__main__":
    collate_in_files(**get_args())
