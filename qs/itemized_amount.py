#!/usr/bin/python3

import datetime
import traceback
import functools
import numbers

import qsutils

STOP_AT_DUPLICATES = False
LARGE_DEBIT = -250

def date_in_year(date):
    return date.strftime("%m:%d") if isinstance(date, datetime.date) else date[-5:]

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
            + " to " + row.get('payee', "unknown payee") \
            + " in " + row.get('category', "unknown category") + ">"

def diagnostic_amount_string(am):
    return repr(am) if isinstance(am, itemized_amount) else "fl %.2f" % am

def compact_row_string(item):
    return "%s %s %s: %s" % (date_in_year(item['date']), item['category'], item['payee'], diagnostic_amount_string(item['amount']))

def row_annotation(row):
    message = row.get('message', "")
    return row.get('category', "unknown category") + ((" " + message)
                                                      if message and message != ""
                                                      else "")

def tooltip_string(item, with_time=False):
    return (('''        <tr><td>%s %s</td><td>%s</td><td>%s</td><td>%s</td></tr>'''
             % (item.get('date', "unknown date"),
                item.get('time', "unknown date"),
                qsutils.tidy_for_output(item.get('amount', "unknown amount")),
                item.get('payee', "unknown payee"),
                row_annotation(item)))
            if with_time
            else ('''        <tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>'''
                  % (item.get('date', "unknown date"),
                     qsutils.tidy_for_output(item.get('amount', "unknown amount")),
                     item.get('payee', "unknown payee"),
                     row_annotation(item))))

def as_number(x):
    return x.as_number() if isinstance(x, itemized_amount) else x

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
        self.amount = transaction['amount'] if transaction else 0
        self.transactions = ([transaction]
                             if isinstance(transaction, dict)
                             else (transaction
                                   if isinstance(transaction, list)
                                   else []))

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
        # print("    rendering cell", repr(self))
        # print("    with transactions", self.transactions)
        self.normalize()
        # print("    rendering normalized cell", repr(self))
        # print("    with normalized transactions", self.transactions)
        dups = self.count_duplicates()
        dupstring = (' <span class="duplicated">{%d}</span>' % dups) if dups else ''
        return ('<td class="%s">' % css_class
                + '<span class="overview%s">%s' % (self.magnitude_class(title, extra_data), str(self))
                + ' <span class="ic">[%d%s]</span>\n' % (len(self.transactions), dupstring)
                + '      <span class="details"><table border>\n'
                + ('''<tr><th colspan="4">%s (%s in %d items)</th></tr>'''
                          % (title, str(self), len(self.transactions)))
                + ('\n'.join([tooltip_string(item, with_time)
                              for item in self.transactions]))
                + '</table></span>\n</span></td>')
