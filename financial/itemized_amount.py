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
    return (row.get('date'), row.get('time'),
            row.get('payee', "unknown payee"),
            amount,
            row.get('category', "unknown category"),
            row.get('unique_number'))

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

class DuplicateItem(Exception):

    def __init__(self, item, incoming, existing):
        self.item = item
        self.incoming = incoming
        self.existing = existing

@functools.total_ordering
class itemized_amount:

    """A financial amount resulting from a group of transactions.

    The individual transactions are recorded as well as the overall amount.

    The overall result can be treated as a number."""

    def __init__(self, transaction=None):
        self.amount = as_number(transaction)
        self.transactions = ([transaction.copy()]
                             if isinstance(transaction, dict)
                             else (transaction.copy()
                                   if isinstance(transaction, list)
                                   else (transaction.transactions
                                         if isinstance(transaction, itemized_amount)
                                         else [])))

    def as_number(self):
        return self.amount if isinstance(self.amount, numbers.Number) else self.amount.as_number()

    def count_duplicates(self):
        return len(self.transactions) - len(set([row_summary(row) for row in self.transactions]))

    def duplicates(self):
        acc = {}
        for row in self.transactions:
            summary = row_summary(row)
            acc[summary] = acc.get(summary, 0) + 1
        return [summary for summary, count in acc.items() if count > 1]

    def __str__(self):
        return (("%.2F" % self.amount)
                if isinstance(self.amount, float)
                else str(self.amount))

    def __repr__(self):
        dups = self.count_duplicates()
        return "<%s (%d%s: {%s})>" % (self.__str__(),
                                             len(self.transactions),
                                             (" (%d dups)" % dups) if dups else "",
                                      ", ".join([compact_row_string(item) for item in self.transactions]))

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
        # print("copying", repr(self))
        result = itemized_amount(self)
        # print("resulting in", repr(result))
        return result

    def normalize(self):
        amount = self.amount
        if isinstance(amount, itemized_amount):
            self.amount = amount.amount
            if len(self.transactions) == 0:
                self.transactions = amount.transactions
            else:
                first = self.transactions[0]
                first_ts = first['timestamp']
                payee = first['payee']
                numeric = self.amount
                matched = False
                if first_ts.day == 1 and first_ts.hour == 0:
                    if len(self.transactions) == 1:
                        matched = True
                    else:
                        for row in amount.transactions:
                            if (row['amount'] == numeric
                                and row['payee'] == payee):
                                matched = True
                                break
                if matched:
                    self.transactions = amount.transactions
                else:
                    self.transactions += amount.transactions

    def add(self, other, depth=0):
        self.normalize()
        # print("  " * depth, "adding", repr(other), "onto", repr(self))
        # traceback.print_stack()
        original_amount = self.as_number()
        if isinstance(other, itemized_amount):
            for item in other.transactions:
                # print("  " * depth, "adding sub-transaction", item)
                self.add(item, depth+1)
        elif isinstance(other, dict):
            # print("  " * depth, "considering adding row")
            if other not in self.transactions:
                # print("  " * depth, "adding row", row_descr(other), "to", repr(self))
                incoming = other['amount']
                if isinstance(incoming, itemized_amount):
                    self.add(incoming, depth+1)
                else:
                    self.amount += incoming
                    self.transactions.append(other)
        elif isinstance(other, list):
            for item in other:
                # print("  " * depth, "adding list element", item)
                self.add(item, depth+1)
        elif isinstance(other, numbers.Number):
            # print("  " * depth, "adding bare number", other)
            self.amount += other
            self.transactions.append({'amount': other})
        # else:
        #     print("  " * depth, "cannot add", other, "to an itemized amount")
        # print("  " * depth, "result of adding", row_descr(other) if isinstance(other, dict) else repr(other), "to", original_amount, "is", repr(self))

    def __add__(self, other):
        result = itemized_amount()
        result.add(self)
        result.add(other)
        return result

    def __radd__(self, other):
        result = itemized_amount()
        result.add(other)
        result.add(self)
        return result

    def __iadd__(self, other):
        dup_before = self.count_duplicates()
        if dup_before:
            print("Already got duplicates before adding")
            raise DuplicateItem(None, self, other)
        self.add(other)
        dup_after = self.count_duplicates()
        if dup_after != dup_before:
            print("__iadd__ caused", dup_after - dup_before, "new duplicates")
            print("duplicates are", self.duplicates())
            if True:
                raise DuplicateItem(None, self, other)
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
        self.normalize()
        dups = self.count_duplicates()
        dupstring = (' <span class="duplicated">{%d}</span>' % dups) if dups else ''
        return ('<td class="%s">' % css_class
                + '<span class="overview%s">%s' % (self.magnitude_class(title, extra_data), str(self))
                + ' <span class="ic">[%d%s]</span>\n' % (len(self.transactions), dupstring)
                + '      <div class="details">\n        <table border>\n'
                + ('''          <tr><th colspan="5" class="dethead">%s (%s in %d items)</th></tr>\n'''
                          % (title, str(self), len(self.transactions)))
                + ('\n'.join([tooltip_string(item, with_time)
                              for item in self.transactions]))
                + '\n        </table>\n      </div>\n    </td>')
