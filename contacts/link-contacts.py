#!/usr/bin/env python

import argparse
from backports import csv
import io
import random

fieldnames = ['Given name', 'Middle names', 'Surname', 'Title', 'Old name', 'AKA',
              'Birthday',
              'Gender',
              'ID', 'Parents', 'Offspring', 'Siblings', 'Partners', 'Ex-partners', 'Nationality',
              'Notes',
              'Group Membership', 'Other groups', 'Organizations', 'Subjects', 'Jobs',
              'Primary email', 'Other emails',
              'Phone 1 Type', 'Phone 1 Value',
              'Phone 2 Type', 'Phone 2 Value',
              'Street', 'City', 'Region', 'Postal Code', 'Country', 'Extended Address']

def make_name(person):
    return ' '.join([person.get('Given name', "")]
                    + person.get('Middle names', "").split()
                    + [person.get('Surname', "")])

def make_ID():
    return (str(unichr(random.randint(0, 19) + ord('G')))
            + str(unichr(random.randint(0, 9) + ord('0')))
            + str(unichr(random.randint(0, 25) + ord('A')))
            + str(unichr(random.randint(0, 9) + ord('0'))))

def offspring(person):
    return person['Offspring']

def parents(person):
    return person['Parents']

def partners(person):
    return person['Partners']

def siblings(person):
    return person['Siblings']

def name(person):
    return person['_name_']

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--analyze", action='store_true')
    parser.add_argument("input")
    parser.add_argument("output")
    args = parser.parse_args()
    by_id = {}
    without_id = []
    by_name = {}
    by_nationality = {}
    by_gender = {}
    by_title = {}
    with io.open(args.input, 'r', encoding='utf-8') as input:
        contacts_reader = csv.DictReader(input)
        for row in contacts_reader:
            row['Parents'] = row.get('Parents', "").split()
            row['Offspring'] = row.get('Offspring', "").split()
            row['Siblings'] = row.get('Siblings', "").split()
            row['Partners'] = row.get('Partners', "").split()
            row['Organizations'] = row.get('Organizations', "").split()
            n = make_name(row)
            row['_name_'] = n
            by_name[n] = row
            id = row.get('ID', "")
            if id != "":
                by_id[id] = row
            else:
                without_id.append(row)

    for person in without_id:
        id = make_ID()
        while id in by_id:
            id = make_ID()
        person['ID'] = id
        by_id[id] = person

    for id, person in by_id.iteritems():
        by_nationality.get(person['Nationality'], []).append(id)
        by_gender.get(person['Gender'], []).append(id)
        by_title.get(person['Title'], []).append(id)

    if args.analyze:
        print len(by_nationality), "nationalities:", ", ".join(by_nationality.keys())

    for nm in sorted(by_name.keys()):
        person = by_name[nm]
        person_id = person['ID']
        for parent_id in parents(person):
            parent = by_id[parent_id]
            if person_id not in offspring(parent):
                offspring(parent).append(person_id)
        for offspring_id in offspring(person):
            child = by_id[offspring_id]
            if person_id not in parents(child):
                parents(child).append(person_id)
        for sibling_id in siblings(person):
            sibling = by_id[sibling_id]
            if person_id not in siblings(sibling):
                siblings(sibling).append(person_id)

    with io.open(args.output, 'w', encoding='utf-8') as output:
        contacts_writer = csv.DictWriter(output, fieldnames)
        contacts_writer.writeheader()
        for nm in sorted(by_name.keys()):
            row = by_name[nm]
            row['Parents'] = ' '.join(row['Parents'])
            row['Offspring'] = ' '.join(row['Offspring'])
            row['Siblings'] = ' '.join(row['Siblings'])
            row['Partners'] = ' '.join(row['Partners'])
            del row['_name_']
            contacts_writer.writerow(row)

if __name__ == "__main__":
    main()
