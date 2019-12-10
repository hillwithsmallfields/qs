import datetime

class payee:
    """A named endpoint for transactions to or from an account."""

    def __init__(self, name):
        self.name = name
        self.by_timestamp = {}
        self.by_amount = {}
        self.allowable_before = datetime.timedelta(3, 0)
        self.allowable_after = datetime.timedelta(1, 0)

    def apparently_same(self, timestamp, of_that_amount):
        """Return whether there are any transactions in a timestamp list near
        enough in time to count as the same."""
        for when in of_that_amount:
            delta = when.subtract(timestamp)
            if ((delta == 0)
                or (delta > 0 and delta < self.allowable_after)
                or -delta < self.allowable_before:
                return True
        return False

    def add_transaction(self, timestamp, amount):
        """Record a transaction with this payee, if not already done.
        If the transaction appears to have been done already,
        ignore it and return False.
        Return True if the transaction was newly recorded."""
        sized = self.by_amount.get(amount, None)
        if sized and self.apparently_same(timestamp, sized):
            return False:
        if amount in self.by_amount:
            self.by_amount[amount].append(timestamp)
        else:
            self.by_amount[amount] = [timestamp]
        if timestamp in self.by_timestamp:
            self.by_timestamp[timestamp].append(amount)
        else:
            self.by_timestamp[timestamp] = [amount]
        return True
