import account
import base_sheet
import canonical_sheet
import csv
import functools
import operator
import os
import qsutils

class category_tree:

    """The results of splitting an account into its categories."""

    def __init__(self, original_account=None):
        self.categories = {}    # by leaf category, to dicts of timestamp to transaction
        self.summaries = {}     # by parent categories, to dicts of timestamp to transaction
        self.config = None
        if original_account:
            self.config = original_account.config
            self.add_from_account(original_account)

    def __repr__(self):
        # return ("<category_tree "
        #         + ", ".join([key + ": "
        #                      + str(qsutils.tidy_for_output(functools.reduce(operator.add,
        #                                                                   [transaction['amount']
        #                                              for transaction in self.categories[key]])))
        #                      for key in sorted(self.categories.keys())])
        #         + ">")
        return ("<category_tree " + ", ".join(self.categories.keys()) + ">")

    def add_from_account(self, incoming_account):
        print("category_tree.add_from_account", incoming_account)
        if isinstance(incoming_account, account.account):
            print("category_tree.add_from_account from account")
            for timestamp, transaction in incoming_account.all_transactions.items():
                self.categorise_transaction(transaction)
        elif isinstance(incoming_account, canonical_sheet.canonical_sheet):
            print("category_tree.add_from_account from canonical_sheet")
            for timestamp, row in incoming_account.rows.items():
                self.categorise_transaction(row)
        else:
            print("Don't know how to add inputs of type", type(incoming_account), "to a category_tree")
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
        combined = category_tree()
        combined.config = self.config
        combined.categories = {k: qsutils.merge_by_date(v, date_chars)
                               for k, v in self.categories.items()}
        combined.summaries = {k: qsutils.merge_by_date(v, date_chars)
                              for k, v in self.summaries.items()}

    def write_csv(self, filename):
        """Write a category sheet to a CSV file.
        Each row is a category and its total payments."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        qsutils.ensure_directory_for_file(full_filename)
        with open(full_filename, 'w') as outfile:
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
                       qsutils.tidy_for_output(
                           qsutils.sum_amount(cat.values())),
                       len(cat)]
                writer.writerow(row)

class categorised_sheet(base_sheet.base_sheet):

    """A sheet with dates for the rows and categories for the keys."""

    def __init__(self, config, incoming_data=None):
        super().__init__(config)
        print("making categorised_sheet from incoming_data", incoming_data)
        if isinstance(incoming_data, canonical_sheet.canonical_sheet):
            print("making categorised_sheet: from canonical_sheet to account")
            incoming_data = account.account("categorised", base_account=incoming_data)
        if isinstance(incoming_data, account.account):
            print("making categorised_sheet: from account to category tree")
            incoming_data = category_tree(incoming_data)
        if isinstance(incoming_data, category_tree):
            print("making categorised_sheet: adding from tree", incoming_data)
            self.add_from_tree(incoming_data)

    def add_category(self, catname, cat):
        for timestamp, transaction in cat.items():
            if timestamp not in self.rows:
                self.rows[timestamp] = {}
            if catname not in self.rows[timestamp]:
                self.rows[timestamp][catname] = {}
            self.rows[timestamp][catname].append(cat)

    def add_from_tree(self, incoming_tree):
        for cat in incoming_tree.categories.items():
            print("adding category item", cat)
            add_category(catname, cat)
        for cat in incoming_tree.summaries.items():
            print("adding summary item", cat)
            add_category(catname, cat)

    def write_csv(self, filename):
        # todo: convert the lists of transactions to sums
        self.write_all_columns(filename)
