#!/usr/bin/env python3
# Fill in details in my towers list, from Dove

import csv
import os

DOVE_FILE = os.path.expanduser("~/Downloads/dove.csv")

MY_TOWERS_FILE = os.path.expanduser("~/Sync/ringing/towers.csv")

def tower_names(tower):
    return [tower['PlaceCL'] or tower['Place'],
            tower['Place'],
            tower['AltName'] or tower['Place'],
            "%s, %s" % (tower['Place'], tower['Dedicn']),
            "%s (%s)" % (tower['Place'], tower['County']),
            ]

def read_dove():
    with open(DOVE_FILE) as dovestream:
        return {
            name: tower
            for tower in csv.DictReader(dovestream)
            for name in tower_names(tower)
        }

def read_visits():
    with open(MY_TOWERS_FILE) as towerstream:
        return {
            tower['Tower']: tower
            for tower in csv.DictReader(towerstream)
        }

transfer_keys = {
    'County': 'County',
    'Diocese': 'Diocese',
    'Lat': 'Latitude',
    'Long': 'Longitude',
    'Bells': 'Bells',
    'Wt': 'Lbs',
    }

OUT_COLUMNS = ['Tower', 'Date', 'Bells', 'Lbs', 'Diocese', 'County', 'Latitude', 'Longitude']

def towers_fill_in():
    dove = read_dove()
    visits = read_visits()

    for name, visit in visits.items():
        if name in dove:
            extra_details = dove[name]
            for dove_column, visit_column in transfer_keys.items():
                visit[visit_column] = extra_details[dove_column]
        else:
            print("No details for", name)
    with open("/tmp/towers.csv", 'w') as outstream:
        writer = csv.DictWriter(outstream, OUT_COLUMNS)
        writer.writeheader()
        for name in sorted(visits.keys()):
            writer.writerow(visits[name])

if __name__ == "__main__":
    towers_fill_in()
