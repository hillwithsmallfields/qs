"""Categorize financial entries."""

import numbers
import os
import yaml

def parentages(cats):
    parents = dict()
    def descend(parent, these, pars):
        for item, subtree in these.items():
            parents[item] = parent
            descend(item, subtree, pars)
    descend(None, cats, parents)
    return parents

def earliest_ancestor(cat, parentage):
    return (earliest_ancestor(parentage[cat], parentage)
            if (cat in parentage and parentage[cat])
            else cat)

def add_top_ancestor(table):
    with open(os.path.expandvars("$SYNCED/finances/cats.yaml")) as ystream:
        parentage = parentages(yaml.safe_load(ystream))
    for row in table:
        row["Class"] = earliest_ancestor(row["Category"], parentage)
    return table

def abs_if_num(x):
    return abs(x) if isinstance(x, numbers.Number) else x

def spread(table, selector_column, value_column):
    return [{row[selector_column]: abs_if_num(row[value_column]),
             'Date': row['Date'],
             'Date number': row.get('Date number')}
            for row in table]
