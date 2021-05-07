#!/usr/bin/python3

import argparse
import csv
import os

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--journeys", "-j",
                        default="$COMMON/travel/travel.csv")
    parser.add_argument("--places", "-p",
                        default="$COMMON/travel/places/places.csv")
    args = parser.parse_args()

    with open(os.path.expandvars(args.places)) as places_stream:
        places = {row['Place']: row for row in csv.DictReader(places_stream)}

    with open(os.path.expandvars(args.journeys)) as journey_stream:
        journeyer = csv.reader(journey_stream)
        for date, destination in journeyer:
            print(destination, date)

if __name__ == '__main__':
    main()
