#!/usr/bin/python3
# program to scrape myfitnesspal, using https://github.com/coddingtonbear/python-myfitnesspal

import argparse
import csv
from decouple import config
import datetime
import json
import myfitnesspal
import random
import time

client = None

meal_keys = ['breakfast', 'lunch', 'dinner', 'snacks']
meal_numbers = {v:i for i,v in enumerate(meal_keys)}

def all_empty(data):
    for meal in data.values():
        if len(meal) > 0:
            return False
    return True

def fetch_streak_upto(when,
                      accumulator={}, sheet={},
                      countdown=None):
    global client
    if client is None:
        client = myfitnesspal.Client(config('MFP_USERNAME'), config('MFP_PASSWORD'))
    while True:
        date_key = when.date()
        if date_key not in accumulator:
            day_data = client.get_date(when.year, when.month, when.day)
            dict_data = day_data.get_as_dict()
            if all_empty(dict_data):
                break
            accumulator[date_key] = dict_data
            row = {mealname+'_cals': day_data.meals[i]['calories'] for i, mealname in meal_keys}
            row.update(day_data.totals)
            row['date' = date_key]
            sheet[date_key] = row
            if countdown:
                countdown -= 1
                if countdown == 0:
                    break
        when = when - datetime.timedelta(days=1)
        time.sleep(random.randint(1, 5))
    return accumulator

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--number", "-n",
                        type=int, default=0,
                        help="""Maximum number of days to fetch.""")
    parser.add_argument("--update", "-u",
                        action='store_true',
                       help="""Read the existing file contents and add to them.""")
    parser.add_argument("--latest", "-l",
                        help="""The most recent date to fetch.""")
    parser.add_argument("--sheet", "-p")
    parser.add_argument("--json", "-o")
    args = parser.parse_args()

    so_far = {}
    if args.update and args.json:
        with open(args.json) as instream:
            so_far = {datetime.datetime.fromisoformat(k): v
                      for k, v in json.load(instream).items()}

    rows = {}
    if args.update and args.sheet:
        with open(args.sheet) as instream:
            rows = {row['date']: row
                     for row in csv.DictReader(instream)}

    upto_date = (datetime.datetime.fromisoformat(args.latest)
                 if args.latest
                 else datetime.datetime.today())

    fetch_streak_upto(upto_date,
                      accumulator=so_far,
                      sheet=rows,
                      countdown=(args.number
                                      if args.number > 0
                                      else None))

    if args.json:
        with open(args.json, 'w') as outstream:
            json.dump({d.isoformat(): v
                       for d,v in so_far.items()},
                      outstream)

    if args.sheet:
        with open(args.sheet, 'w') as outstream:
            fieldnames = ['date',
                          'breakfast', 'lunch', 'dinner', 'snacks',
                          'calories',
                          'carbohydrates', 'fat', 'protein', 'sodium', 'sugar']
            writer = csv.DictWriter(outstream, fieldnames=fieldnames)
            writer.writeheader()
            for row in rows:
                writer.writerow(row)

if __name__ == "__main__":
    main()
