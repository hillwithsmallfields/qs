import account
import base_sheet
import canonical_sheet
import csv
import functools
import operator
import os
import qsutils

class CategoryTree:

    """The results of splitting an account into its categories."""

    def __init__(self, original_account=None):
        self.categories = {}
        self.config = None
        if original_account:
            self.config = original_account.config
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

    def summarize(original_tree):
        """Produce a version of a category tree with the summary entries added.

        Summaries are entries for each level of parent entry, with all
        the transactions of their children combined.

        """
        # todo: write this
        pass

    def combine_same_period_entries(self,
                                    period,
                                    time_chars=19,
                                    comment=None):
        """Produce an category tree based on this one, with just one entry per period
        (day, by default).

        For each category, convert all the entries in the same period to one total."""
        # todo: write this
        pass

    def write_csv(self, filename):
        """Write a category sheet to a CSV file.
        Each row is a category and its total payments."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            colseq = ['category','child', 'parentage', 'total', 'count']
            writer = csv.writer(outfile, colseq)
            writer.writerow(colseq)
            for cat_name in sorted(self.categories.keys()):
                cat = self.categories[cat_name]
                as_list = cat_name.split(':')
                row = [cat_name,
                       as_list[-1],
                       ':'.join(as_list[:-1]),
                       qsutils.trim_if_float(
                           functools.reduce(
                               operator.add,
                               [transaction['amount'] for transaction in cat])),
                       len(cat)]
                writer.writerow(row)
