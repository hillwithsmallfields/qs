import datetime

class payee:
    """A named endpoint for transactions to or from an account."""

    def __init__(self, name):
        self.name = name
        self.by_timestamp = {}
        self.by_amount = {}
        self.allowable_before = datetime.timedelta(3, 0)
        self.allowable_after = datetime.timedelta(1, 0)

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
        if sized and self.timestamp_matches_one_in_list(timestamp, sized):
            return True

    def add_transaction(self, timestamp, amount):
        """Record a transaction with this payee.
        You should first check that it is not a duplicate,
        using self.already_seen."""
        sized = self.by_amount.get(amount, None)
        if amount in self.by_amount:
            self.by_amount[amount].append(timestamp)
        else:
            self.by_amount[amount] = [timestamp]
        if timestamp in self.by_timestamp:
            self.by_timestamp[timestamp].append(amount)
        else:
            self.by_timestamp[timestamp] = [amount]
