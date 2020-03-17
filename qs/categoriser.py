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
        self.categories = {}    # by leaf category
        self.summaries = {}     # by parent categories
        self.config = None
        if original_account:
            self.config = original_account.config
            self.add_from_account(original_account)

    def __repr__(self):
        # return ("<CategoryTree "
        #         + ", ".join([key + ": "
        #                      + str(qsutils.trim_if_float(functools.reduce(operator.add,
        #                                                                   [transaction['amount']
        #                                              for transaction in self.categories[key]])))
        #                      for key in sorted(self.categories.keys())])
        #         + ">")
        return ("<CategoryTree " + ", ".join(self.categories.keys()) + ">")

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
        timestamp = transaction['timestamp']
        if parent:
            category = parent + ":" + category
        if category in self.categories:
            self.categories[category][timestamp] = transaction
        else:
            self.categories[category] = {timestamp: transaction}
        if parent:
            ancestors = parent.split(':')
            for i in range(len(ancestors)):
                ak = tuple(ancestors[:i+1])
                if ak in self.summaries:
                    self.summaries[ak][timestamp] = transaction
                else:
                    self.summaries[ak] = {timestamp: transaction}

    def combine_same_period_entries(self,
                                    period,
                                    date_chars=10,
                                    comment=None):
        """Produce an category tree based on this one, with just one entry per period
        (day, by default).

        For each category, convert all the entries in the same period to one total.
        Likewise for the summaries (parents)."""
        combined = CategoryTree.CategoryTree()
        combined.config = self.config
        combined.categories = {k: qsutils.merge_by_date(v, time_chars)
                               for k, v in self.categories.items()}
        combined.summaries = {k: qsutils.merge_by_date(v, time_chars)
                              for k, v in self.summaries.items()}

    def write_csv(self, filename):
        """Write a category sheet to a CSV file.
        Each row is a category and its total payments."""
        with open(os.path.expanduser(os.path.expandvars(filename)),
                  'w') as outfile:
            colseq = ['category','child', 'parentage', 'total', 'count']
            writer = csv.writer(outfile, colseq)
            writer.writerow(colseq)
            for cat_name in sorted(self.categories.keys()):
                cat = self.categories[cat_name]
                # print("cat_name", cat_name)
                # for transaction in cat.values():
                #     print("    ", transaction)
                as_list = cat_name.split(':')
                # print("will try to sum thing of type", type(cat.values()))
                row = [cat_name,
                       as_list[-1],
                       ':'.join(as_list[:-1]),
                       qsutils.trim_if_float(
                           qsutils.sum_amount(cat.values())),
                       len(cat)]
                writer.writerow(row)
