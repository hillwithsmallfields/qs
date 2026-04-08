import csv
import os
import requests
from collections import defaultdict

MY_TOWERS_FILE = os.path.expanduser("~/Sync/ringing/towers.csv")

# The columns to write in my tower visits records
OUT_COLUMNS = ['Tower',
               'Date',
               'Bells',
               'Weight',
               'Lbs',
               'Diocese',
               'County',
               'Latitude',
               'Longitude']

# The columns to copy from the reference Dove file to my personal
# tower visits file, with the column names to use in the output:
TRANSFER_KEYS = {
    'County': 'County',
    'Diocese': 'Diocese',
    'Lat': 'Latitude',
    'Long': 'Longitude',
    'Bells': 'Bells',
    'Wt': 'Lbs',
    }

def read_visits():
    """Read my tower visits records."""
    with open(MY_TOWERS_FILE) as towerstream:
        return {
            tower['Tower']: tower
            for tower in csv.DictReader(towerstream)
        }

def write_visits(visits):
    """Read my tower visits records."""
    with open(MY_TOWERS_FILE, 'w') as outstream:
        writer = csv.DictWriter(outstream, OUT_COLUMNS)
        writer.writeheader()
        for name in sorted(visits.keys()):
            writer.writerow(visits[name])
    return visits

def towers_fill_in(dove, visits):
    """Fill in details of my tower visits, using the Dove data."""
    for name, visit in visits.items():
        if name in dove:
            towers_matching_name = dove[name]
            if len(towers_matching_name) > 1:
                if visit.get('Weight'):
                    print("Warning: the tower name", name, "is ambiguous but the entry is already complete")
                else:
                    print("Warning: the tower name", name, "is ambiguous, so we can't complete the entry")
                continue
            extra_details = towers_matching_name[0]
            for dove_column, visit_column in TRANSFER_KEYS.items():
                visit[visit_column] = extra_details[dove_column]
        else:
            print("No details for", name)
        if (lbs_text := visit.get('Lbs')):
            lbs = int(lbs_text)
            visit['Weight'] = "%d-%d-%d" % (lbs // 112, (lbs % 112) // 28, lbs % 28)

def classify_towers(visits):
    """Classify my tower visits by:
    - number of bells
    - tenor weight
    - year of first recorded visit."""
    by_bells = defaultdict(int)
    by_weight = defaultdict(int)
    by_year = defaultdict(int)
    for visit in visits.values():
        try:
            by_weight[int(visit['Weight'].split('-')[0])] += 1
            by_bells[int(visit['Bells'])] += 1
            if visit['Date']:
                by_year[int(visit['Date'].split('-')[0])] += 1
        except:
            print("Problem with tower visit", visit)
    return by_bells, by_weight, by_year
