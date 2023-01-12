#!/usr/bin/python3

import finutils
from collections import defaultdict

def add_parentage(tree, table, ancestry):
    """Processes the parentage of a tree level."""
    for parent, children in tree.items():
        if len(children) == 0:
            table[parent] = ancestry
        else:
            add_parentage(children, table, [parent] + ancestry)

def read_parentage_table(parentage_filename):
    """Reads the parentage table.
    The result is a dictionary of category to list of parent categories.
    The parent categories are given nearest-first."""
    table = dict()
    add_parentage(finutils.read_yaml(parentage_filename), table, [])
    return table

def highlights(parentage_table, selection, other='other'):
    """Returns a dictionary mapping categories in input table to those in the selection."""
    return defaultdict(lambda: other,
                       **{cat: list(pars)[0]
                          for cat, pars in {category: set(parents) & selection
                                            for category, parents in parentage_table.items()}.items()
                          if pars})

def read_budgetting_classes_table(classes_filename):
    """Returns a dictionary mapping categories to budgetting groups, from a file."""
    return budgetting_classes_table(finutils.read_yaml(classes_filename))

def budgetting_classes_table(classes, other='other'):
    """Returns a dictionary mapping categories to budgetting groups."""
    return defaultdict(lambda: other,
                       {origin: destination
                        for destination, group in classes.items()
                        for origin in list(group)})

def main():
    """Test program for read_parentage_table."""
    import os.path
    all_entries = read_parentage_table(os.path.expanduser(finutils.CATPARENTS))
    print("All:")
    for key, value in all_entries.items():
        print("  ", key, "->", value)
    some_entries = highlights(all_entries, set(['Food and sundries', 'Charitable donation', 'Clothes']))
    print("Highlights:")
    for key, value in some_entries.items():
        print("    ", key, "=>", value)
    classes = read_budgetting_classes_table(finutils.BUDGETCATS)
    print("From budgetting table")
    for key, value in classes.items():
        print("    ", key, "==>", value)

if __name__ == "__main__":
    main()
