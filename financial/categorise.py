"""Categorize financial entries."""

import numbers
import os
import yaml

def parentages(cats):
    parents = dict()
    print("setting up parentage")
    def descend(parent, these, pars):
        for item, subtree in these.items():
            print("  parent of", item, "is", parent)
            parents[item] = parent
            descend(item, subtree, pars)
    descend(None, cats, parents)
    return parents

def nearest_ancestor_in_selection(cat, parentage, selection):
    """Return the most recent ancestor of a category
    (as shown in the parentage) that is in the selection.
    """
    ancestor = parentage.get(cat)
    return (ancestor
            if (ancestor in selection
                or ancestor is None)
            else nearest_ancestor_in_selection(ancestor, parentage, selection))

def make_map_to_selection(parentage, selection):
    return {category: nearest_ancestor_in_selection(category, parentage, selection)
            for category in parentage.keys()} | {x: x for x in selection}

def abs_if_num(x):
    return abs(x) if isinstance(x, numbers.Number) else x

def spread(table, selector_column, value_column):
    return [{row[selector_column]: abs_if_num(row[value_column]),
             'Date': row['Date'],
             'Date number': row.get('Date number')}
            for row in table]
