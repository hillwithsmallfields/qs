#!/usr/bin/env python3

import argparse
import functools
import operator
import os
import sys

import dobishem
import expressionive.exprpages
from expressionive.expressionive import htmltags as T

import qsutils.qsutils

import financial.finutils
import financial.parentage
import financial.collate

# Used to get the stylesheet and in-page scripts:
source_dir = os.path.dirname(os.path.realpath(__file__))

def entry_as_html(entry, row, column, total):
    cell = row[column]
    return T.div(class_='details')[
        T.table(border=True)[
            T.tr[T.th(colspan=5, class_='dethead')["{}: {} ({:.2f} in {} items)".format(row['Date'], column, total, len(cell))]],
            [
                T.tr[
                    T.td(clas='detamt')["{:.2f}".format(float(item['Amount']))],
                    T.td(clas='detdate')[item['Date']],
                    T.td(clas='detpay')[item['Payee']],
                    T.td(clas='detcat')[item['Category']],
                    T.td(clas='detcat')[item['Item']], # TODO: suppress if this is one of the categories?
                ]
                for item in cell]
            ]
        ]

def cell_total(cell):
    return functools.reduce(
        operator.add,
        (float(item['Amount'])
         for item in cell)) if len(cell) > 0 else 0

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
    row_values = [cell_total(row[column])
                  for row in table]
    return (functools.reduce(operator.add,
                             row_values)
            if row_values
            else 0)

def spending_chart(transactions, key, period, columns, map_to_highlights, thresholds={}):
    """Create a spending chart, as an untemplate structure."""
    result = financial.collate.collate(transactions,
                                       key, period,
                                       column_mapping=map_to_highlights)
    for row in result:
        if 'Other' in row:
            del row['Other']
    totals = {column: column_total(result, column) for column in columns}
    return T.table[
        T.tr[T.th["Date"],
             [T.th[column] for column in columns]],
        [T.tr[T.th[row['Date']],
              [cell_as_html(row, column, thresholds.get(column, 0))
               for column in columns]]
         for row in sorted(result, key=lambda r: r['Date'])],
        T.tr[T.th["Total"],
             [T.td["%.2f" % totals[column]] for column in columns]],
        T.tr[T.th["Daily"],
             [T.td["%.2f" % (totals[column]/365)] for column in columns]],
    ]

def spending_chart_to_file(incoming, key, period, output,
                           thresholds=None,
                           details_background_color="gold",
                           inline=True,
                           all_cats=False):
    """Collates a table from a list, with the output to file."""
    category_mapping = financial.parentage.read_budgetting_classes_table(financial.finutils.BUDGETCATS)
    with open(output, 'w') as page_stream:
        page_stream.write(
            expressionive.exprpages.page_text(
                spending_chart(
                    incoming,
                    key, period,
                    sorted(list(set(category_mapping.values()))),
                    category_mapping,
                    thresholds or {}),
                style_text=((expressionive.exprpages.tagged_file_contents("style",
                                                                          os.path.join(source_dir, "../dashboard/dashboard.css"))
                             + qsutils.qsutils.table_support_css(details_background_color))
                            if inline
                            else ""),
                script_text=(expressionive.exprpages.tagged_file_contents("script",
                                                                          os.path.join(source_dir, "../dashboard/dashboard.js"))
                             if inline
                             else "")))
    # if not inline:
    #     for filename in ("dashboard.css",
    #                      "dashboard.js"):
    #         shutil.copy(os.path.join(source_dir, filename),
    #                     os.path.join(charts_dir, filename))
