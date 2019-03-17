#!/usr/bin/env python
import argparse
from backports import csv
import io
import operator
import os
import re

def read_books(books_file):
    books = {}
    with io.open(books_file, 'r', encoding='utf-8') as input:
        books_reader = csv.DictReader(input)
        for row in books_reader:
            ex_libris = row['Number']
            if ex_libris != "":
                books[int(ex_libris)] = row
    return books

def book_matches(book, pattern):
    pattern = re.compile(pattern , re.IGNORECASE)
    return (pattern.search(book['Title'])
            or pattern.search(book['Authors'])
            or pattern.search(book['Publisher'])
            or pattern.search(book['ISBN'])
            or pattern.search(book['Area']))

def books_matching(book_index, pattern):
    return [ book
             for book in book_index.values()
             if book_matches(book, pattern) ]

def list_books(locations, books):
    # todo: print table of where all books are
    pass

def read_inventory(inventory_file):
    inventory = {}
    unlabelled = -1
    with io.open(inventory_file, 'r', encoding='utf-8') as input:
        inventory_reader = csv.DictReader(input)
        for row in inventory_reader:
            label_number = row['Label number']
            if label_number != "":
                inventory[int(label_number)] = row
            else:
                inventory[unlabelled] = row
                unlabelled -= 1
    return inventory

def item_matches(item, pattern):
    pattern = re.compile(pattern , re.IGNORECASE)
    return (pattern.search(item['Item'])
            or pattern.search(item['Type'])
            or pattern.search(item['Subtype']))

def items_matching(inventory_index, pattern):
    return [ item
             for item in inventory_index.values()
             if item_matches(item, pattern) ]

def list_items(locations, items):
    # todo: option to print table of where all inventory items are
    pass

def read_locations(locations_file):
    locations = {}
    with io.open(locations_file, 'r', encoding='utf-8') as input:
        storage_reader = csv.DictReader(input)
        for row in storage_reader:
            contained_within = row['ContainedWithin']
            row['ContainedWithin'] = (int(contained_within)
                                      if contained_within != ""
                                      else None)
            number = row['Number']
            if number != "":
                locations[int(number)] = row
    return locations

def nested_location(locations, location):
    result = []
    location = int(location)
    while location:
        if location not in locations:
            break
        where = locations[location]
        description = where['Description']
        level = where['Level']
        if level != "":
            description += " "
            if re.match("[0-9]+", level):
                description += "level " + level
            else:
                description += level
        storage_type = where['Type']
        if storage_type not in ("", "room", "building"):
            if (storage_type == "shelf"
                and not re.search("shelves", description)):
                description += " " + storage_type
        result.append(description)
        location = where['ContainedWithin']
    return result

def describe_location(locations, location):
    return (" which is in ".join(nested_location(locations, location))
            if location != ""
            else "unknown")

def analyze_locations(locations):
    capacities = {}
    for location in locations.values():
        loctype = location['Type'].lower()
        locsize = location['Size'].lower()
        if loctype != "" and locsize != "":
            capacities[loctype] = float(capacities.get(loctype, 0)) + float(locsize)
    for loctype in sorted(capacities.keys()):
        print loctype.rjust(max(*map(len, capacities.keys()))), capacities[loctype]
    volume = reduce(operator.add,
                    map(lambda loctype: capacities.get(loctype, 0),
                        ('box', 'crate', 'drawer', 'cupboard')))
    length = reduce(operator.add,
                    map(lambda loctype: capacities.get(loctype, 0),
                        ('shelf', 'shelves', 'cupboard shelf', 'racklevel')))
    area = reduce(operator.add,
                  map(lambda loctype: capacities.get(loctype, 0),
                        ('louvre panel', 'pegboard')))
    print "Total container volume:", volume, "litres"
    print "Total shelving length:", length, "metres"
    print "Total panel area:", area, "square metres"

def list_locations(locations):
    # todo: option to print table of all storage locations, with everything that is in them
    pass

def main():
    parser = argparse.ArgumentParser()
    org_files = os.environ.get("ORG", "~/org")
    parser.add_argument("--locations", "-f",
                        default=os.path.join(org_files, "storage.csv"),
                        help="""The CSV file containing the storage locations.""")
    parser.add_argument("--books", "-b",
                        default=os.path.join(org_files, "books.csv"),
                        help="""The CSV file containing the book catalogue.""")
    parser.add_argument("--inventory", "-i",
                        default=os.path.join(org_files, "inventory.csv"),
                        help="""The CSV file containing the inventory.""")
    parser.add_argument("--list-books", action='store_true',
                        help="""List all the books.""")
    parser.add_argument("--list-items", action='store_true',
                        help="""List all the inventory items.""")
    parser.add_argument("--list-locations", action='store_true',
                        help="""List all the storage locations.""")
    parser.add_argument("--analyze", action='store_true',
                        help="""Analyze the storage.""")
    parser.add_argument("things",
                        nargs='*',
                        help="""The things to look for.""")
    args = parser.parse_args()
    locations = read_locations(args.locations)
    inventory = read_inventory(args.inventory)
    books = read_books(args.books)
    if args.analyze:
        analyze_locations(locations)
    if args.list_books:
        list_books(locations, books)
    elif args.list_items:
        list_items(locations, items)
    elif args.list_locations:
        list_locations(locations)
    else:
        # todo: collect up all the books/items matching all the "thing"s, eliminating duplicates, then output them, as currently if something matches more than one "thing", it appears more than once in the output
        for thing in args.things:
            if re.match("[0-9]+", thing):
                as_location = describe_location(locations, thing)
                if as_location != []:
                    print "as location:", as_location
                # todo: also look up books and items by number
            for book in books_matching(books, thing):
                shelf = book['Location']
                where = describe_location(locations, shelf)
                print "book:", book['Title'], "is on", where
            for item in items_matching(inventory, thing):
                shelf = item['Normal_location']
                where = describe_location(locations, shelf)
                print "item:", item['Item'], "is in", where
            # todo: try each name as a storage location name, and if found, list everything in that location

if __name__ == "__main__":
    main()
