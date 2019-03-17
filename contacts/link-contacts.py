#!/usr/bin/env python

import argparse
from backports import csv
import io
import random
import re
import operator

fieldnames = ['Given name', 'Middle names', 'Surname', 'Title', 'Old name', 'AKA',
              'Birthday', 'Died',
              'First contact', 'Last contact',
              'Gender',
              'ID', 'Parents', 'Offspring', 'Siblings', 'Partners', 'Ex-partners', 'Nationality',
              'Notes',
              'Group Membership', 'Other groups', 'Organizations', 'Place met',
              'Subjects', 'Jobs',
              'Primary email', 'Other emails',
              'Primary phone Type', 'Primary phone Value',
              'Secondary phone Type', 'Secondary phone Value',
              'Street', 'City', 'Region', 'Postal Code', 'Country', 'Extended Address']

# Fields to split into lists
multi_fields = ['Parents', 'Offspring', 'Siblings', 'Partners', 'Organizations']

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

by_name = {}

def normalize_to_IDs(people):
    return [ (person
              if re.match("[G-Z][0-9][A-Z][0-9]", person)
              else by_name[person.replace('_', ' ')]['ID'])
             for person in people ]

def find_siblings(person, by_id):
    sibs = siblings(person)
    for sib in siblings(person):
        sibsibs = siblings(by_id[sib])
        for sibsib in sibsibs:
            if sibsib not in sibs:
                sibs.append(sibsib)
    yourself = person['ID']
    if yourself in sibs:
        sibs.remove(yourself)
    return sibs

def name(person):
    return person['_name_']

def group_titles(title_map, titles):
    return reduce(operator.add, map(len, map(lambda(title): title_map[title], titles)))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--analyze", action='store_true')
    parser.add_argument("input")
    parser.add_argument("output")
    args = parser.parse_args()
    global by_name
    by_id = {}
    without_id = []
    by_nationality = {}
    by_gender = {}
    by_title = {}
    with io.open(args.input, 'r', encoding='utf-8') as input:
        contacts_reader = csv.DictReader(input)
        for row in contacts_reader:
            for multi in multi_fields:
                row[multi] = row.get(multi, "").split()
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

    for person in by_id.values():
        person['Parents'] = normalize_to_IDs(parents(person))
        person['Offspring'] = normalize_to_IDs(offspring(person))
        person['Siblings'] = normalize_to_IDs(siblings(person))
        person['Partners'] = normalize_to_IDs(partners(person))

    for person_id, person in by_id.iteritems():
        for parent_id in parents(person):
            parent = by_id[parent_id]
            if person_id not in offspring(parent):
                offspring(parent).append(person_id)
        for offspring_id in offspring(person):
            child = by_id[offspring_id]
            if person_id not in parents(child):
                parents(child).append(person_id)
        for sibling_id in find_siblings(person, by_id):
            sibling = by_id[sibling_id]
            if person_id not in siblings(sibling):
                siblings(sibling).append(person_id)

    if args.analyze:
        for id, person in by_id.iteritems():
            nationality = person['Nationality']
            if nationality not in by_nationality:
                by_nationality[nationality] = []
            by_nationality[nationality].append(id)
            gender = person['Gender']
            if gender not in by_gender:
                by_gender[gender] = []
            by_gender[gender].append(id)
            title = person['Title']
            if title not in by_title:
                by_title[title] = []
            by_title[title].append(id)
        n_people = len(by_id)
        print n_people, "people"
        print len(by_nationality), "nationalities:", ", ".join([k + "(" + str(len(by_nationality[k])) + ")" for k in sorted(by_nationality.keys())])
        print len(by_gender), "genders:", ", ".join([k + "(" + str(len(by_gender[k])) + ")" for k in sorted(by_gender.keys())])
        print len(by_title), "titles:", ", ".join([k + "(" + str(len(by_title[k])) + ")" for k in sorted(by_title.keys())])
        ordained = group_titles(by_title, ["Revd", "Revd Dr", "Revd Prof", "RtRevd"])
        doctored = group_titles(by_title, ["Dr", "Revd Dr", "Prof", "Revd Prof"])
        print "%d ordained (%d%% of the people you know)" % (ordained, ordained*100 / n_people)
        print "%d with doctorates (%d%% of the people you know)" % (doctored, doctored * 100 / n_people)

    with io.open(args.output, 'w', encoding='utf-8') as output:
        contacts_writer = csv.DictWriter(output, fieldnames)
        contacts_writer.writeheader()
        for nm in sorted(by_name.keys()):
            row = by_name[nm]
            for multi in multi_fields:
                row[multi] = ' '.join(row[multi])
            del row['_name_']
            contacts_writer.writerow(row)

if __name__ == "__main__":
    main()
