import datetime
import qsutils

class payee:
    """A named endpoint for transactions to or from an account."""

    def __init__(self, name):
        self.name = name
        self.by_timestamp = {}  # dictionary of timestamps to lists of amounts
        self.by_amount = {}     # dictionary of amounts to lists of timestamps
        self.balance = 0
        self.ordered = None
        self.cursor = -1
        self.current = None
        self.subcursor = -1
        self.allowable_before = datetime.timedelta(3, 0)
        self.allowable_after = datetime.timedelta(1, 0)

    def __str__(self):
        return ("<payee " + ("unknown" if self.name == "" else self.name)
                + " balance " + str(self.balance) + ">")

    def __iter__(self):
        self.ordered = sorted(self.by_amount.keys())
        self.cursor = -1        # because we pre-increment it
        return self

    def __next__(self):
        self.subcursor += 1
        if self.current is None or self.subcursor >= len(self.current):
            self.cursor += 1
            if self.cursor >= len(self.ordered):
                raise StopIteration
            self.current = self.order[self.cursor]
            self.subcursor = 0
        return (self.cursor, self.current[self.subcursor])

    def transactions_string(self, separator=',', time_chars=19):
        """Return a string representing the transactions for this payee."""
        return separator.join([
            ('@'
             + str(ts)[:time_chars]
             + ': '
             + separator.join([qsutils.trim_if_float(a)
                               for a in self.by_timestamp[ts]]))
            for ts in sorted(self.by_timestamp.keys())])

    def timestamp_matches_one_in_list(self, timestamp, of_that_amount):
        """Return whether there are any transactions in a timestamp list near
        enough in time to count as the same."""
        for when in of_that_amount:
            delta = when - timestamp
            if (when == timestamp
                or (when > timestamp and delta < self.allowable_after)
                or (-delta < self.allowable_before)):
                return True
        return False

    def already_seen(self, timestamp, amount):
        """Return whether a transaction of a given amount, around a given
        time, matches any to this payee.
        """
        sized = self.by_amount.get(amount, None)
        return sized and self.timestamp_matches_one_in_list(timestamp, sized)

    def add_transaction(self, timestamp, amount):
        """Record a transaction with this payee.
        You should first check that it is not a duplicate,
        using self.already_seen."""
        if amount in self.by_amount:
            self.by_amount[amount].append(timestamp)
        else:
            self.by_amount[amount] = [timestamp]
        if timestamp in self.by_timestamp:
            self.by_timestamp[timestamp].append(amount)
        else:
            self.by_timestamp[timestamp] = [amount]
        self.balance += amount
