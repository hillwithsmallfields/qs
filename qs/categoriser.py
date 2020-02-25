import account
import canonical_sheet
import functools
import operator
import qsutils

class CategoryTree:

    """The results of splitting an account into its categories."""

    def __init__(self, original_account=None):
        self.categories = {}
        if original_account:
            self.add_from_account(original_account)

    def __repr__(self):
        return ("<CategoryTree "
                + ", ".join([key + ": "
                             + str(qsutils.trim_if_float(functools.reduce(operator.add,
                                                                          [transaction['amount']
                                                     for transaction in self.categories[key]])))
                             for key in sorted(self.categories.keys())])
                + ">")

    def add_from_account(self, incoming_account):
        if isinstance(incoming_account, account.account):
            for timestamp, transaction in incoming_account.all_transactions.items():
                self.categorise_transaction(transaction)
        elif isinstance(incoming_account, canonical_sheet.canonical_sheet):
            for timestamp, row in incoming_account.rows.items():
                self.categorise_transaction(row)
        else:
            print("Don't know how to add inputs of type", type(incoming_account), "to a CategoryTree")
            return None

    def categorise_transaction(self, transaction):
        if 'category' not in transaction:
            return
        category = transaction['category']
        parent = transaction.get('parent', None)
        if parent:
            category = parent + ":" + category
        if category in self.categories:
            self.categories[category].append(transaction)
        else:
            self.categories[category] = [transaction]
