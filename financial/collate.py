#!/usr/bin/env python3

import argparse
from collections import defaultdict
import datetime
import functools
import operator

import finutils

def collate(incoming, key, period):

    period_fn = {
        'day': lambda isodate: isodate,
        'month': lambda isodate: isodate[:7],
        'year': lambda isodate: isodate[:4],
        'weekday': lambda isodate: str(datetime.date.fromisoformat(isodate).weekday()),
    }[period]

    result = defaultdict(lambda: defaultdict(list))
    for row in incoming:
        result[period_fn(row['date'])][row[key]].append(row)
    # for outkey, outrow in result.items():
    #     print(outkey, outrow)
    return finutils.with_key_as_column(result, 'date')

def summarize(table):
    return [
        {
            column: "{:.2f}".format(functools.reduce(operator.add, (float(item['amount']) for item in cell)))
            for column, cell in row.items() if column != 'date'
        } | {'date': row['date']}
        for row in table
    ]

def textualize(table):
    return [
        {
            column: "; ".join(("{} {} {}".format(item['date'], item['payee'], item['amount']) for item in cell))
            for column, cell in row.items() if column != 'date'
        } | {'date': row['date']}
        for row in table
    ]

def collate_in_files(incoming, key, period, summary, output):
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
