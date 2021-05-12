#!/usr/bin/python3

import argparse
import csv
import os
import requests
import time

def places_main(places_file, update):
    with open(os.path.expandvars(places_file)) as places_stream:
        places = {row['Place']: row for row in csv.DictReader(places_stream)}

    if update:
        for place in places.values():
            if not (place.get('Longitude') and place.get('Latitude')):

                url = "https://nominatim.openstreetmap.org/search?format=geojson&addressdetails=1&q=%s" % (place['Place'])
                county = place.get('County', None)
                if county and county != "":
                    url += "," + county
                country = place.get('Country', None)
                if country and country != "":
                    url += "," + country

                time.sleep(1)

                placelist = requests.get(url).json()

                matching_places = [q for q in [(p['properties'], p['geometry']['coordinates']) for p in placelist['features']] if q[0].get('category') == 'place']

                if len(matching_places) == 1:
                    description = matching_places[0][0]
                    location = matching_places[0][1]
                    place['Longitude'] = location[0]
                    place['Latitude'] = location[1]
                    if 'address' in description:
                        address = description['address']
                        if 'county' in address:
                            place['County'] = address['county']
                        if 'country' in address:
                            place['Country'] = address['country']
                elif len(matching_places) > 1:
                    print("Ambiguous data for", place)
                    for mp in matching_places:
                        mp_descr = mp[0]
                        if 'address' in mp_descr:
                            print("  ", mp_descr['address'])
                        else:
                            print("  ", mp)
                else:
                    print("No data for", place)

        with open("/tmp/places.csv",
                # os.path.expandvars(places_file),
                'w') as places_stream:
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
