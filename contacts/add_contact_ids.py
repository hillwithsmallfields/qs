#!/usr/bin/env python

import argparse
from backports import csv
import io
import random

fieldnames = ['Surname', 'Given name', 'Title', 'AKA',
              'Birthday',
              'Gender',
              'ID', 'Parents', 'Partners',
              'Notes',
              'Group Membership', 'Other groups',
              'E-mail 1', 'E-mail 2',
              'Phone 1 Type', 'Phone 1 Value',
              'Phone 2 Type', 'Phone 2 Value',
              'Street', 'City', 'Region', 'Postal Code', 'Country', 'Extended Address']    =isaiah=:/home/jcgs/$             + str(unichr(random.randint(0, 9) + ord('0'))))
def make_ID():
    return (str(unichr(random.randint(0, 19) + ord('g')))
            + str(unichr(random.randint(0, 9) + ord('0')))
            + str(unichr(random.randint(0, 25) + ord('a')))
            + str(unichr(random.randint(0, 9) + ord('0'))))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input")
    parser.add_argument("output")
    args = parser.parse_args()
    with io.open(args.input, 'r', encoding='utf-8') as input:
        contacts_reader = csv.DictReader(input)
        with io.open(args.output, 'w', encoding='utf-8') as output:
            contacts_writer = csv.DictWriter(output, fieldnames)
            contacts_writer.writeheader()
            for row in contacts_reader:
                id = row.get('ID', ""):
                if id == "":
                    row['ID'] = make_ID()
                contacts_writer.writerow(row)

if __name__ == "__main__":
    main()
