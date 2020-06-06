#!/usr/bin/python3
# program to scrape myfitnesspal, using https://github.com/coddingtonbear/python-myfitnesspal

import sys
import os
sys.path.append(os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/python-myfitnesspal/myfitnesspal"))

# print("path is now", sys.path)

import argparse
import base_sheet
import qsutils
import csv
from decouple import config
import datetime
import json
import myfitnesspal
import random
import time

client = None

def ensure_mfp_login():
    """Ensure that we are connected to MyFitnessPal."""
    global client
    if client is None:
        client = myfitnesspal.Client(config('MFP_USERNAME'), config('MFP_PASSWORD'))

meal_keys = ['breakfast', 'lunch', 'dinner', 'snacks']
meal_numbers = {v:i for i,v in enumerate(meal_keys)}

def all_empty(data):
    """Return whether all the meals in that data are empty."""
    for meal in data.values():
        if len(meal) > 0:
            return False
    return True

def meal_calories(day_data, meal_index):
    return day_data.meals[meal_index].totals.get('calories', 0)

def fetch_streak_upto(when,
                      accumulator, sheet,
                      countdown=None,
                      verbose=False,
                      minpause=5,
                      maxpause=30):
    """Fetch a continuous series of days' data, from the specified date
    going back until there is a break, or until a given number of days
    have been fetched.

    """
    ensure_mfp_login()
    if verbose:
        print("now logged in")
    while True:
        if ((accumulator is not None and when not in accumulator)
            or (sheet is not None and when not in sheet)):
            if verbose:
                print("getting data for", when)
            day_data = client.get_date(when.year, when.month, when.day)
            dict_data = day_data.get_as_dict()
            cardio = day_data.exercises[0].get_as_list()
            if verbose:
                print("day_data is", day_data, "and dict_data is", dict_data, "and cardio is", cardio)
            if all_empty(dict_data):
                if verbose:
                    print("couldn't get any data for", when)
                break
            if accumulator is not None:
                if verbose:
                    print("adding", dict_data, "to json")
                accumulator[when] = dict_data
            if sheet is not None:
                if verbose:
                    print("accumulating spreadsheet data")
                row = {mealname+'_cals': meal_calories(day_data, i)
                       for i, mealname in enumerate(meal_keys)}
                row['cardio_minutes'] = sum([exercise['nutrition_information']['minutes'] for exercise in cardio])
                row['cardio_calories'] = sum([exercise['nutrition_information']['calories burned'] for exercise in cardio])
                row.update(day_data.totals)
                row['date'] = when
                if verbose:
                    print("adding", row, "to sheet with date", when)
                sheet[when] = row
            if verbose:
                print("countdown was", countdown)
            if countdown:
                countdown -= 1
                if verbose:
                    print("countdown is", countdown)
                if countdown <= 0:
                    break
        when = when - datetime.timedelta(days=1)
        pause = random.randint(minpause, maxpause)
        if verbose:
            print("pausing", pause, "before fetching previous day")
        time.sleep(pause)
    return accumulator

def find_last_unfetched_date(dict_by_date):
    """Return the latest day before the first that appears as a key of a dict.
    This is for working your way back through your historical record,
    not for updating since the last run."""
    return min(dict_by_date.keys()) - datetime.timedelta(days=1)

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("-N", "--number",
                        type=int, default=0,
                        help="""Maximum number of days to fetch.""")
    parser.add_argument("-u", "--update",
                        action='store_true',
                       help="""Read the existing file contents and add to them.""")
    parser.add_argument("-l", "--latest",
                        help="""The most recent date to fetch.""")
    parser.add_argument("-a", "--autodate-old",
                        action='store_true',
                        help="""Work out how far back to start automatically (with --update only).
                        This is for working your way back through your historical record,
                        not for updating since the last run.""")
    parser.add_argument("-p", "--sheet")
    parser.add_argument("-o", "--json")
    parser.add_argument("--minpause",
                        type=int,
                        default=5,
                        help="""The minimum time to pause between day fetches.""")
    parser.add_argument("--maxpause",
                        type=int,
                        default=30,
                        help="""The minimum time to pause between day fetches.""")
    args = parser.parse_args()

    if args.sheet is None and args.json is None:
        print("No output specified!  Use --sheet or --json or both.")
        return 1

    configuration = qsutils.program_load_config(args)

    so_far = {}
    if args.update and args.json:
        with open(args.json) as instream:
            so_far = {datetime.date.fromisoformat(k): v
                      for k, v in json.load(instream).items()}

    rows = {}
    if args.update and args.sheet:
        with open(args.sheet) as instream:
            rows = {datetime.date.fromisoformat(row['date']): row
                     for row in csv.DictReader(instream)}
            
    upto_date = (datetime.date.fromisoformat(args.latest)
                 if args.latest
                 else (find_last_unfetched_date(rows or so_far)
                       if (args.autodate_old and args.update)
                       else datetime.datetime.today()))
    
    fetch_streak_upto(upto_date,
                      accumulator=so_far if args.json else None,
                      sheet=rows if args.sheet else None,
                      countdown=(args.number
                                      if args.number > 0
                                      else None),
                      verbose=args.verbose,
                      minpause=args.minpause,
                      maxpause=args.maxpause)

    if args.json:
        with open(args.json, 'w') as outstream:
            json.dump({d.isoformat(): v
                       for d,v in so_far.items()},
                      outstream)

    if args.sheet:
        base_sheet.base_sheet(configuration, rows=rows).write_csv(args.sheet)

if __name__ == "__main__":
    main()
