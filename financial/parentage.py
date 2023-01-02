#!/usr/bin/python3

import finutils

def add_parentage(tree, table, ancestry):
    """Process the parentage of a tree level."""
    for parent, children in tree.items():
        if len(children) == 0:
            table[parent] = ancestry
        else:
            add_parentage(children, table, [parent] + ancestry)

def read_parentage_table(parentage_filename):
    """Read the parentage table.
    The result is a dictionary of category to list of parent categories.
    The parent categories are given nearest-first."""
    table = dict()
    add_parentage(finutils.read_yaml(parentage_filename), table, [])
    return table

def main():
    """Test program for read_parentage_table."""
    for key, value in read_parentage_table("../conf/cats.yaml").items():
        print(key, value)

if __name__ == "__main__":
    main()
