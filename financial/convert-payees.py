#!/usr/bin/python3

import csv
import yaml
import os

def main():
    with open(os.path.expanduser("~/Sync/finances/to-convert.csv")) as payees_in:
        payees = {x['Statement']: x for x in csv.DictReader(payees_in)}
    with open (os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/qs/cats.csv")) as cats_in:
        categories = {x['category']: x for x in csv.DictReader(cats_in)}
    with open(os.path.expanduser("~/Sync/finances/conversions.yaml")) as conv_in:
        formats = yaml.safe_load(conv_in)
        print("keys of formats are", [k for k in formats.keys()])
        conversions = formats['formats']['handelsbanken']['conversions']
    for payee in payees.values():
        as_statement = payee['Statement']
        as_app = payee['Name']
        category = payee['Category']
        parent = categories.get(category, None)
        if parent is None and category is not None:
            count = 0
            for p in categories.keys():
                if p.startswith(category):
                    parent = categories[p]
                    count += 1
            if count > 1:
                print(category, "is ambiguous")
            elif count == 0:
                print(category, "not found")
        if parent:
            parent = parent['parent']
        # print(as_statement, "becomes", as_app, "with default category", category, "which has parent", parent)
        payee['parent'] = parent
        if as_statement in conversions:
            print("Already got", as_statement)
        else:
            conversions[as_statement] = {'payee': as_app,
                      'category': category,
                      'parent': parent or ""}
    with open(os.path.expanduser("~/Sync/finances/conversions-new.yaml"), 'w') as output_stream:
        yaml.dump(formats, output_stream)

if __name__ == "__main__":
    main()
