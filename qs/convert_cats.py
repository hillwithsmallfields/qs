#!/usr/bin/python3

import argparse
import csv
import json
import yaml

def register_line(line, tree, depth=0):
    # print("  " * depth, line, tree)
    head = line[0]
    if head not in tree:
        tree[head] = {}
    subtree = tree[head]
    if len(line) > 1:
        register_line(line[1:], subtree, depth+1)

def read_csv_cats(filename):
    tree = {}
    with open(filename) as instream:
        for row in csv.DictReader(instream):
            line = list(row['parent'].split(':'))
            line.append(row['category'])
            register_line(list(line), tree)
    return tree

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output', '-o')
    parser.add_argument('infile')
    args = parser.parse_args()
    if args.infile.endswith('.csv'):
        cats = read_csv_cats(args.infile)
    elif args.infile.endswith('.json'):
        with open(args.infile) as instream:
            cats = json.load(instream)
    elif args.infile.endswith('.yaml'):
        with open(args.infile) as instream:
            cats = yaml.load(instream)
    else:
        print("Could not read", args.infile)
        exit(1)
    if args.output.endswith('.json'):
        with open(args.output, 'w') as outstream:
            json.dump(cats, outstream, indent=4)
    elif args.output.endswith('.yaml'):
        with open(args.output, 'w') as outstream:
            outstream.write(yaml.dump(cats))

if __name__ == "__main__":
    main()
