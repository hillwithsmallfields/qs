#!/usr/bin/python3

import datetime
import functools
import numbers
import os
import sys
import traceback

qs_project = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
sys.path.append(os.path.join(qs_project, "utils"))

import qsutils

STOP_AT_DUPLICATES = False
LARGE_DEBIT = -250

def date_in_year(date):
    return date.strftime("%m:%d") if isinstance(date, datetime.date) else date[-5:]

def date_in_full(date):
    return date.isoformat() if isinstance(date, datetime.date) else date

FULL_TS_IN_COMPACT = True

def date_for_compact(date):
    return date_in_full(date) if FULL_TS_IN_COMPACT else date_in_year(date)

def row_summary(row):
    amount = row['amount']
    if isinstance(amount, itemized_amount):
        amount = amount.amount
    return tuple((row.get('date'), row.get('time'),
                  row.get('payee', "unknown payee"),
                  amount,
                  row.get('category', "unknown category"),
                  row.get('unique_number')))

def row_descr(row):
    return "<item " \
            + row['timestamp'].isoformat() \
            + " " + str(row['amount']) \
            + " to " + (row.get('payee') or "unknown payee") \
            + " in " + row.get('category', "unknown category") \
            + ((" for " + row['item'])
               if ('item' in row
                   and row['item'] is not None
                   and row['item'] != "")
               else "") \
            + ">"

def diagnostic_amount_string(am):
    return repr(am) if isinstance(am, itemized_amount) else "fl %.2f" % am

def compact_row_string(item):
    if 'date' not in item or 'category' not in item or 'amount' not in item:
        return str(item)
    return "%s %s%s %s: %s" % (date_for_compact(item['date']),
                               item['category'],
                               (" (%s)" % item['item']) if 'item' in item and item['item'] != "" else "",
                               item['payee'],
                               diagnostic_amount_string(item['amount']))

def row_annotation(row):
    item = row.get('item', "")
    return row.get('category', "unknown category") + ((" " + item)
                                                      if item and item != ""
                                                      else "")

def tooltip_string(item, with_time=False):
    return (('''          <tr><td class="detdate">%s %s</td><td class="detamt">%s</td><td class="detpay">%s</td><td class="detcat">%s</td><td class="detitem">%s</td></tr>'''
             % (item.get('date', "date?"),
                item.get('time', "time?"),
                qsutils.tidy_for_output(item.get('amount', "amount?")),
                item.get('payee', "payee?"),
                item.get('category', "?"),
                item.get('item', "")))
            if with_time
            else ('''          <tr><td class="detdate">%s</td><td class="detamt">%s</td><td class="detpay">%s</td><td class="detcat">%s</td><td class="detitem">%s</td></tr>'''
                  % (item.get('date', "date?"),
                     qsutils.tidy_for_output(item.get('amount', "amount?")),
                     item.get('payee', "payee?"),
                     item.get('category', "?"),
                     item.get('item', ""))))

def as_number(x):
    try:
        return (0
                if not x
                else (x.as_number()
                      if isinstance(x, itemized_amount)
                      else (x['amount']
                            if isinstance(x, dict)
                            else x)))
    except Exception as e:
        print("Could not get a number from", x, "because of", e)

@functools.total_ordering
class itemized_amount:

    """A financial amount resulting from a group of transactions.

    The individual transactions are recorded as well as the overall amount.

    The overall result can be treated as a number."""

    def __init__(self, transaction=None):
        self.amount = 0
        self.transactions = {}
        self.add(transaction)

    def as_number(self):
        return self.amount if isinstance(self.amount, numbers.Number) else self.amount.as_number()

    def __str__(self):
        return (("%.2F" % self.amount)
                if isinstance(self.amount, float)
                else str(self.amount))

    def __repr__(self):
        return "<%s {%d: %s}>" % (self.__str__(),
                              len(self.transactions),
                              ", ".join([compact_row_string(self.transactions[key])
                                         for key in sorted(self.transactions.keys())]))

    def __eq__(self, other):
        if other is None or other == 'None':
            return False
        return (abs(self.amount - other) < 0.001
                if isinstance(other, numbers.Number)
                else abs(self.amount - other.amount) < 0.001)

    def __lt__(self, other):
        return self.amount < (other.amount
                              if isinstance(other, itemized_amount)
                              else other)

    def __abs__(self):
        a = itemized_amount(self.transactions)
        a.amount = abs(self.as_number())
        return a

    def copy(self):
        return itemized_amount(self)

    def add_row(self, row):
        """Add a row (item) to this itemized amount.
        This flattens any inner itemized_amounts in the 'transactions' field."""
        amount = row.get('amount')
        if isinstance(amount, itemized_amount):
            for subrow in amount.values():
                self.add_row(subrow)
        else:
            self.amount += as_number(amount)
            key = row_summary(row)
            self.transactions[key] = row.copy()

    def add(self, other):
        if isinstance(other, dict):
            self.add_row(other)
        elif isinstance(other, list):
            for subtrans in other:
                self.add_row(subtrans)
        elif isinstance(other, itemized_amount):
            for subtrans in other.transactions.values():
                self.add_row(subtrans)
        elif isinstance(other, numbers.Number):
            self.add_row({'amount': other})
        elif other is not None:
            print("cannot add", other, "to an itemized amount")

    def __add__(self, other):
        result = itemized_amount(self)
        result.add(other)
        return result

    def __radd__(self, other):
        result = itemized_amount(other)
        result.add(self)
        return result

    def __iadd__(self, other):
        self.add(other)
        return self

    def __truediv__(self, other):
        result = itemized_amount()
        divisor = other.amount if isinstance(other, itemized_amount) else other
        result.amount = float('NaN') if divisor == 0 else self.amount / divisor
        result.transactions = self.transactions # maybe we should scale all their amounts by divisor?
        return result

    def magnitude_class(self, colname, thresholds):
        """Return a supplement to the overview class name, based on the size of the amount."""
        return (" large"
                if self.amount < (-abs(thresholds)
                                  if isinstance(thresholds, numbers.Number)
                                  else LARGE_DEBIT)
                else (" credit"
                      if self.amount > 0
                      else ""))

    def html_cell(self, css_class, title, extra_data=None, with_time=False):
        """Return the text of an HTML cell describing this itemized amount.
        It includes an item list that may be styled as a tooltip by CSS."""
        dups = self.count_duplicates()
        dupstring = (' <span class="duplicated">{%d}</span>' % dups) if dups else ''
        return ('<td class="%s">' % css_class
                + '<span class="overview%s">%s' % (self.magnitude_class(title, extra_data), str(self))
                + ' <span class="ic">[%d%s]</span>\n' % (len(self.transactions), dupstring)
                + '      <div class="details">\n        <table border>\n'
                + ('''          <tr><th colspan="5" class="dethead">%s (%s in %d items)</th></tr>\n'''
                          % (title, str(self), len(self.transactions)))
                + ('\n'.join([tooltip_string(self.transactions[key], with_time)
                              for key in sorted(self.transactions.keys())]))
                + '\n        </table>\n      </div>\n    </td>')
