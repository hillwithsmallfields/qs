#!/usr/bin/env python3
# Fill in details in my towers list, from Dove

import csv
import os
from collections import defaultdict

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

OUT_COLUMNS = ['Tower', 'Date', 'Bells', 'Weight', 'Lbs', 'Diocese', 'County', 'Latitude', 'Longitude']

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
        if 'Lbs' in visit and visit['Lbs']:
            lbs = int(visit['Lbs'])
            visit['Weight'] = "%d-%d-%d" % (lbs // 112, (lbs % 112) // 28, lbs % 28)
    with open("/tmp/towers.csv", 'w') as outstream:
        writer = csv.DictWriter(outstream, OUT_COLUMNS)
        writer.writeheader()
        for name in sorted(visits.keys()):
            writer.writerow(visits[name])

    by_bells = defaultdict(int)
    by_weight = defaultdict(int)
    by_year = defaultdict(int)
    for visit in visits.values():
        by_weight[int(visit['Weight'].split('-')[0])] += 1
        by_bells[int(visit['Bells'])] += 1
        if visit['Date']:
            by_year[int(visit['Date'].split('-')[0])] += 1

    with open("/tmp/by-weight.csv", 'w') as bw_stream:
        writer = csv.writer(bw_stream)
        writer.writerow(['Hundredweight', 'Towers'])
        for cwt in sorted(by_weight.keys(), reverse=True):
            writer.writerow([cwt, by_weight[cwt]])
    with open("/tmp/by-bells.csv", 'w') as bb_stream:
        writer = csv.writer(bb_stream)
        writer.writerow(['Bells', 'Towers'])
        for bells in sorted(by_bells.keys(), reverse=True):
            writer.writerow([bells, by_bells[bells]])
    with open("/tmp/by-year.csv", 'w') as by_stream:
        writer = csv.writer(by_stream)
        writer.writerow(['Year', 'Towers'])
        for year in sorted(by_year.keys(), reverse=True):
            writer.writerow([year, by_year[year]])

if __name__ == "__main__":
    towers_fill_in()
