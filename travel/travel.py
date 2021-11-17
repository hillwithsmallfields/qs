#!/usr/bin/python3

import argparse
import csv
import os

def travel_main(journeys, places):
    with open(os.path.expandvars(places)) as places_stream:
        places = {row['Place']: row for row in csv.DictReader(places_stream)}

    with open(os.path.expandvars(journeys)) as journey_stream:
        journeyer = csv.reader(journey_stream)
        for date, destination in journeyer:
            place_details = places.get(destination)
            print(destination, date, place_details)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--journeys", "-j",
                        default="$SYNCED/travel/travel.csv")
    parser.add_argument("--places", "-p",
                        default="$SYNCED/travel/places/places.csv")
    args = parser.parse_args()

    travel_main(args.journeys, args.places)

if __name__ == '__main__':
    main()
