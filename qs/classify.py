# classify categories into groups, initially for charting etc

import yaml

def read_classifier(classification_file):
    with open(classification_file) as instream:
        classes = yaml.safe_load(instream.read())
    result = {}
    for name, members in classes.items():
        for member in members:
            result[member] = name
    return result

def classify(category, parentage, classes, collect_unknowns=True, pass_unknowns=False):
    """Classify a category.
    If it is directly in any of the classes, put it in that class;
    otherwise if any of its parent categories are in a class, use that.
    The classes are normally a dictionary, but a list may be given,
    in which case membership is tested.
    If pass_unknowns is given as False, None is returned for unclassed categories,
    otherwise they serve as their own classes.
    Passing a list for classes and False for pass_unknowns makes this act as a filter."""
    if category in classes:
        return classes[category] if isinstance(classes, dict) else category
    if parentage:
        for ancestor in reversed(parentage):
            if ancestor in classes:
                return (classes[ancestor]
                        if isinstance(classes, dict)
                        else (category
                              if pass_unknowns
                              else ('Other'
                                    if collect_unknowns
                                    else None)))
    return (category
            if pass_unknowns
            else ('Other'
                  if collect_unknowns
                  else None))
