import datetime
import functools
import operator
import qsutils

class payee:
    """A named endpoint for transactions to or from an account."""

    payee_id = 0

    def __init__(self, name):
        self.name = name
        self.by_timestamp = {}  # dictionary of timestamps to lists of rows
        self.by_amount = {}     # dictionary of amounts to lists of rows
        self.balance = 0
        self.ordered = None
        self.cursor = -1
        self.current = None
        self.subcursor = -1
        self.allowable_before = datetime.timedelta(3, 0)
        self.allowable_after = datetime.timedelta(1, 0)
        self.payee_id = payee.payee_id
        payee.payee_id += 1

    def __str__(self):
        return ("<payee " + ("unknown" if self.name == "" else self.name)
                + " balance " + str(self.balance) + " [p" + str(self.payee_id) + "]>")

    def __iter__(self):
        self.ordered = sorted(self.by_timestamp.keys())
        self.cursor = 0
        self.current = self.by_timestamp[self.ordered[0]]
        self.subcursor = -1
        return self

    def __next__(self):
        self.subcursor += 1
        if self.subcursor >= len(self.current):
            self.cursor += 1
            if self.cursor >= len(self.ordered):
                raise StopIteration
            self.current = self.by_timestamp[self.ordered[self.cursor]]
            self.subcursor = -1
        return (self.ordered[self.cursor], self.current[self.subcursor])

    def transactions_string(self, separator=',', time_chars=19):
        """Return a string representing the transactions for this payee."""
        return separator.join([
            ('@'
             + str(ts)[:time_chars]
             + ': '
             + separator.join([qsutils.trim_if_float(a['amount'])
                               for a in self.by_timestamp[ts]]))
            for ts in sorted(self.by_timestamp.keys())])

    def transactions_total(self):
        "Return the total transactions on this payee."
        return functools.reduce(operator.add,
                                [qsutils.sum_amount(vs)
                                 for vs in self.by_timestamp.values()])

    def timestamp_matches_one_in_list(self, timestamp, of_that_amount):
        """Return whether there are any transactions in a timestamped row list
        near enough in time to count as the same.
        """
        for row in of_that_amount:
            when = row['timestamp']
            delta = when - timestamp
            if (when == timestamp
                or (when > timestamp and delta < self.allowable_after)
                or (-delta < self.allowable_before)): # implicitly when < timestamp
                return row
        return False

    def already_seen(self, timestamp, amount):
        """Return whether a transaction of a given amount, around a given
        time, matches any to this payee.
        """
        sized = self.by_amount.get(amount, None)
        return sized and self.timestamp_matches_one_in_list(timestamp, sized)

    def add_row(self, row):
        """Record a transaction with this payee.
        You should first check that it is not a duplicate,
        using self.already_seen."""
        timestamp = row['timestamp']
        amount = row['amount']
        if amount in self.by_amount:
            self.by_amount[amount].append(row)
        else:
            self.by_amount[amount] = [row]
        if timestamp in self.by_timestamp:
            self.by_timestamp[timestamp].append(row)
        else:
            self.by_timestamp[timestamp] = [row]
        self.balance += amount

    def add_transaction(self, timestamp, amount, origin_sheet,
                        comment=None, flags=None, extra=None):
        row = {'timestamp': timestamp,
               'date': timestamp.date(),
               'time': timestamp.time(),
               'amount': amount,
               'payee': self.name,
               'sheet': origin_sheet}
        if extra:
            for column in ['category', 'parent',
                           'project', 'location',
                           'original_amount', 'original_currency',
                           'message']:
                if column in extra:
                    value = extra[column]
                    if value is not None and value != "" and value != 0:
                        row[column] = value
        if comment:
            row['message'] = comment
        if flags:
            row['flags'] = flags
        self.add_row(row)
