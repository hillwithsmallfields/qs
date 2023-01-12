#!/usr/bin/env python3

import argparse
import functools
import operator
import os
import sys

source_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(os.path.dirname(source_dir))

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

sys.path.append(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

import qsutils.qsutils
import qsutils.html_pages

import finutils
import parentage
import collate

def entry_as_html(entry, row, column, total):
    cell = row[column]
    return T.div(class_='details')[
        T.table(border=True)[
            T.tr[T.th(colspan=5, class_='dethead')["{}: {} ({:.2f} in {} items)".format(row['date'], column, total, len(cell))]],
            [
                T.tr[
                    T.td(clas='detamt')["{:.2f}".format(float(item['amount']))],
                    T.td(clas='detdate')[item['date']],
                    T.td(clas='detpay')[item['payee']],
                    T.td(clas='detcat')[item['category']]
                ]
                for item in cell]
            ]
        ]

def cell_total(cell):
    return functools.reduce(
        operator.add,
        (float(item['amount'])
         for item in cell))

def cell_as_html(row, column, threshold):
    cell = row[column]
    if cell:
        total = cell_total(cell)
        return T.td(class_=column)[[
            T.span(class_='overview' + (
                ' large'
                if threshold and abs(total) > abs(threshold)
                else '') + (' credit'
                            if total > 0
                            else ''))["{:.2f}".format(total),
                               T.span(class_='ic')["[{:d}]".format(len(cell))],
                               [entry_as_html(entry, row, column, total)
                                for entry in cell]]]]
    else:
        return T.td[""]

def column_total(table, column):
    return functools.reduce(
        operator.add,
        (cell_total(row[column])
         for row in table))

def spending_chart(transactions, key, period, columns, map_to_highlights, thresholds):
    """Create a spending chart, as an untemplate structure."""
    result = collate.collate(transactions, key, period, map_to_highlights)
    for row in result:
        if 'Other' in row:
            del row['Other']
    return T.table[
        T.tr[T.th["Date"],
             [T.th[column] for column in columns]],
        [T.tr[T.th[row['date']],
              [cell_as_html(row, column, thresholds.get(column))
               for column in columns]]
         for row in sorted(result, key=lambda r: r['date'])],
        T.tr[T.th["Total"],
             [T.td[column_total(result, column)] for column in columns]],
    ]

def spending_chart_to_file(incoming, key, period, output,
                           starting=None, ending=None,
                           thresholds=None,
                           details_background_color="gold",
                           inline=True,
                           all_cats=False):
    """Collates a table from a file, with the output to file."""
    category_mapping = parentage.read_budgetting_classes_table(finutils.BUDGETCATS)
    with open(output, 'w') as page_stream:
        page_stream.write(
            qsutils.html_pages.page_text(
                spending_chart(
                    finutils.read_csv(incoming, starting, ending),
                    key, period,
                    sorted(list(set(category_mapping.values()))),
                    category_mapping,
                    thresholds or {}),
                ((qsutils.html_pages.tagged_file_contents("style", os.path.join(source_dir, "../dashboard/dashboard.css"))
                 + qsutils.qsutils.table_support_css(details_background_color))
                 if inline
                 else ""),
                (qsutils.html_pages.tagged_file_contents("script", os.path.join(source_dir, "../dashboard/dashboard.js"))
                 if inline
                 else "")))
    # if not inline:
    #     for filename in ("dashboard.css",
    #                      "dashboard.js"):
    #         shutil.copy(os.path.join(source_dir, filename),
    #                     os.path.join(charts_dir, filename))

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--incoming", "-i", default=finutils.MAIN_ACCOUNTING_FILE,
                        help="""The input file, in my financisto-like format.""")
    parser.add_argument("--key", "-k", default='category',
                        help="""The field to group transactions by.
                        'category' and 'payee' are probably the most useful.""")
    parser.add_argument("--period", "-p", default='month',
                        help="""The period to group transactions by.
                        Must be one of 'day', 'month', 'year', or 'weekday'.""")
    parser.add_argument("--starting",
                        help="""Trim the transactions to start at this date.""")
    parser.add_argument("--ending",
                        help="""Trim the transactions to end at this date.""")
    parser.add_argument("--output", "-o",
                        help="""The full output file.""")
    return vars(parser.parse_args())

if __name__ == "__main__":
    spending_chart_to_file(**get_args())
