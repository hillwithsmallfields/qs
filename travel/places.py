#!/usr/bin/python3

import argparse
import os

def places_main(places_file, update):
    with open(os.path.expandvars(places_file)) as places_stream:
        places = {row['Place']: row for row in csv.DictReader(places_stream)}

    if update:
        for place in places.values():
            if not (place.get('Longitude') and place.get('Latitude')):
                # TODO: get coordinates from OSM/Nominatim etc
                pass
        with open(os.path.expandvars(places_file), 'w') as places_stream:
            writer = csv.DictWriter(places_stream, ['Place', 'County', 'Country', 'Longitude', 'Latitude'])
            writer.writeheader()
            for place in places.values():
                writer.writerow(place)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--places", "-p",
                        default="$COMMON/travel/places/places.csv")
    parser.add_argument("--update", "-u",
                        action='store_true')
    args = parser.parse_args()

    places_main(args.places, args.update)

if __name__ == '__main__':
    main()
