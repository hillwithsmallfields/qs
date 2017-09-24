#!/usr/bin/python

# Program to go from my combined body logs to Weight Tracker app format.

# If an existing Weight Tracker file is given, any entries in it are
# skipped from the output, as the point of the program is to fill in
# gaps in the app's data from other data (e.g. from before I started
# to use the app).

import argparse
import csv

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-e", "--existing",
                        help="""The name of a file containing the existing app entries as exported by the app.""")
    parser.add_argument("-o", "--output",
                        required=True,
                        help="""The name of the file to put the output in.""")
    parser.add_argument("data",
                        help="""The main data file.""")
    args = parser.parse_args()
    exclude = {}
    output = []
    if args.existing is not None:
        with open(args.existing) as existing:
            for row in csv.reader(existing):
                if len(row) >= 2:
                    exclude[row[1][:10]] = row[0]
    with open(args.data) as main_file:
        with open(args.output, 'w') as outfile:
            app_writer = csv.writer(outfile)
            for row in csv.DictReader(main_file):
                date = row['Date']
                if date in exclude:
                    continue
                weight = row['Kg']
                if weight == "":
                    continue
                app_writer.writerow([weight, date + " 08:00:00", ""])

if __name__ == "__main__":
    main()
