import csv
import os
import requests
from collections import defaultdict

DOVE_FILE = os.path.expanduser("~/Downloads/dove.csv")
DOVE_URL = "https://dove.cccbr.org.uk/towers.csv"
MY_TOWERS_FILE = os.path.expanduser("~/Sync/ringing/towers.csv")

def tower_names(tower):
    return [tower['PlaceCL'] or tower['Place'],
            tower['Place'],
            tower['AltName'] or tower['Place'],
            "%s, %s" % (tower['Place'], tower['Dedicn']),
            "%s (%s)" % (tower['Place'], tower['County']),
            ]

def read_dove():
    if not os.path.exists(DOVE_FILE):
        print("Downloading tower data from Dove's Guide")
        download = requests.get(DOVE_URL)
        if download.status_code == 200:
            print("Saving Dove data")
            with open(DOVE_FILE, 'w') as dove_save:
                dove_save.write(download.text)
        else:
            print("Failed to fetch Dove data")
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

OUT_COLUMNS = ['Tower', 'Date', 'Bells', 'Weight', 'Lbs', 'Diocese', 'County', 'Latitude', 'Longitude']

def write_visits(visits):
    with open(MY_TOWERS_FILE, 'w') as outstream:
        writer = csv.DictWriter(outstream, OUT_COLUMNS)
        writer.writeheader()
        for name in sorted(visits.keys()):
            writer.writerow(visits[name])

transfer_keys = {
    'County': 'County',
    'Diocese': 'Diocese',
    'Lat': 'Latitude',
    'Long': 'Longitude',
    'Bells': 'Bells',
    'Wt': 'Lbs',
    }

def towers_fill_in(dove, visits):
    """Fill in details of my tower visits, using the Dove data."""
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

def classify_towers(visits):
    by_bells = defaultdict(int)
    by_weight = defaultdict(int)
    by_year = defaultdict(int)
    for visit in visits.values():
        by_weight[int(visit['Weight'].split('-')[0])] += 1
        by_bells[int(visit['Bells'])] += 1
        if visit['Date']:
            by_year[int(visit['Date'].split('-')[0])] += 1
    return by_bells, by_weight, by_year
