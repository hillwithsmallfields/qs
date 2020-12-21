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

def classify(category, parentage, classes):
    if category in classes:
        return classes[category]
    if parentage:
        for ancestor in reversed(parentage):
            if ancestor in classes:
                return classes[ancestor]
    return category
