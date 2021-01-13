#!/usr/bin/python3

import traceback
import functools
import numbers

import qsutils

STOP_AT_DUPLICATES = False

def row_summary(row):
    amount = row['amount']
    if isinstance(amount, itemized_amount):
        amount = amount.amount
    return (row.get('date'), row.get('time'), row.get('payee', "unknown payee"), amount, row.get('category', "unknown category"), row.get('unique_number'))

def row_descr(row):
    return "<item " + row['timestamp'].isoformat() + " " + str(row['amount']) + " to " + row.get('payee', "unknown payee"), " in ", row.get('category', "unknown category") + ">"

def tooltip_string(item, with_time=False):
    return (('''        <tr><td>%s %s</td><td>%s</td><td>%s</td><td>%s</td></tr>'''
             % (item.get('date', "unknown date"),
                item.get('time', "unknown date"),
                qsutils.tidy_for_output(item.get('amount', "unknown amount")),
                item.get('payee', "unknown payee"),
                item.get('category', "unknown category")))
            if with_time
            else ('''        <tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>'''
                  % (item.get('date', "unknown date"),
                     qsutils.tidy_for_output(item.get('amount', "unknown amount")),
                     item.get('payee', "unknown payee"),
                     item.get('category', "unknown category"))))

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
                else str(type(self.amount)) + str(self.amount))

    def __repr__(self):
        dups = self.count_duplicates()
        return "<%s (%d transactions%s)>" % (self.__str__(),
                                             len(self.transactions),
                                             (", %d dups" % dups) if dups else "")

    def __eq__(self, other):
        if other is None or other == 'None':
            return False
        return (abs(self.amount - other) < 0.001
                if isinstance(other, numbers.Number)
                else abs(self.amount - other.amount) < 0.001)

    def __lt__(self, other):
        return self.amount < other.amount

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
        # print("  " * depth, "adding", other, "onto", self)
        self.normalize()
        # traceback.print_stack()
        if isinstance(other, itemized_amount):
            for item in other.transactions:
                # print("  " * depth, "adding sub-transaction", item)
                self.add(item, depth+1)
        elif isinstance(other, dict):
            # print("  " * depth, "considering adding row")
            if other not in self.transactions:
                # print("  " * depth, "adding row")
                incoming = other['amount']
                if isinstance(incoming, itemized_amount):
                    self.add(incoming, depth+1)
                else:
                    self.amount += incoming # <================== I think this is where the bare number adds are coming from; it implies that self.amount is itself an itemized_amount, which I hadn't had in mind; I was thinking of that as a possibility in the incoming ones; I think I should probably normalize these away, but where?
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
        # print("  " * depth, "result of adding", other, "is", self)
            
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

    def html_cell(self, css_class, title, with_time=False):
        dups = self.count_duplicates()
        dupstring = (' <span class="duplicated">{%d}</span>' % dups) if dups else ''
        return ('<td class="%s">' % css_class
                + '<span class="overview">%s' % str(self)
                + ' <span class="ic">[%d%s]</span>\n' % (len(self.transactions), dupstring)
                + '      <span class="details"><table border>\n'
                + ('''<tr><th colspan="4">%s (%s in %d items)</th></tr>'''
                          % (title, str(self), len(self.transactions)))
                + ('\n'.join([tooltip_string(item, with_time)
                              for item in self.transactions]))
                + '</table></span>\n</span></td>')
