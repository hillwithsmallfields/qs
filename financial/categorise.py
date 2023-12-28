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

# No longer used
# def earliest_ancestor(cat, parentage):
#     return (earliest_ancestor(parentage[cat], parentage)
#             if (cat in parentage and parentage[cat])
#             else cat)

# No longer used
# def add_top_ancestor(table):
#     with open(os.path.expandvars("$SYNCED/finances/cats.yaml")) as ystream:
#         parentage = parentages(yaml.safe_load(ystream))
#     for row in table:
#         row["Class"] = earliest_ancestor(row["Category"], parentage)
#     return table

def abs_if_num(x):
    return abs(x) if isinstance(x, numbers.Number) else x

def spread(table, selector_column, value_column):
    return [{row[selector_column]: abs_if_num(row[value_column]),
             'Date': row['Date'],
             'Date number': row.get('Date number')}
            for row in table]
