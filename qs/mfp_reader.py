#!/usr/bin/python3
# program to scrape myfitnesspal, using https://github.com/coddingtonbear/python-myfitnesspal

import sys
import os
sys.path.append(os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/python-myfitnesspal/myfitnesspal"))

print("path is now", sys.path)

import argparse
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
    print("day_data", day_data, "meal_index", meal_index)
    print("meals", day_data.meals)
    print("this meal", day_data.meals[meal_index])
    tot = day_data.meals[meal_index].totals
    print("got totals", tot)
    return tot.get('calories', 0)

def fetch_streak_upto(when,
                      accumulator, sheet,
                      countdown=None,
                      verbose=False):
    """Fetch a continuous series of days' data, from the specified date
    going back until there is a break, or until a given number of days
    have been fetched.

    """
    if verbose:
        print("fetching with accumulator", accumulator, "and sheet", sheet, "and countdown", countdown)
    ensure_mfp_login()
    if verbose:
        print("now logged in")
    while True:
        date_key = when.date()
        print("on date_key", date_key, "for", when)
        if ((accumulator is not None and date_key not in accumulator)
            or (sheet is not None and date_key not in sheet)):
            if verbose:
                print("getting data for", when)
            day_data = client.get_date(when.year, when.month, when.day)
            dict_data = day_data.get_as_dict()
            if verbose:
                print("day_data is", day_data, "and dict_data is", dict_data)
            if all_empty(dict_data):
                if verbose:
                    print("couldn't get any data for", date_key)
                break
            if accumulator is not None:
                if verbose:
                    print("adding", dict_data, "to json")
                accumulator[date_key] = dict_data
            if sheet is not None:
                if verbose:
                    print("accumulating spreadsheet data")
                row = {mealname+'_cals': meal_calories(day_data, i)
                       for i, mealname in enumerate(meal_keys)}
                row.update(day_data.totals)
                row['date'] = date_key
                if verbose:
                    print("adding", row, "to sheet with date", date_key)
                sheet[date_key] = row
            if verbose:
                print("countdown was", countdown)
            if countdown:
                countdown -= 1
                if verbose:
                    print("countdown is", countdown)
                if countdown <= 0:
                    break
        when = when - datetime.timedelta(days=1)
        pause = random.randint(1, 5)
        if verbose:
            print("pausing", pause, "before fetching previous day")
        time.sleep(pause)
    return accumulator

def find_last_unfetched_date(dict_by_date):
    """Return the latest day before the first that appears as a key of a dict."""
    return nil                  # todo: fill this in

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
    parser.add_argument("--autodate", "-a",
                        action='store_true',
                        help="""Work out how far back to start automatically (with --update only).""")
    parser.add_argument("--sheet", "-p")
    parser.add_argument("--json", "-o")
    parser.add_argument("--verbose", "-v",
                        action='store_true',
                        help="""Produce some diagnostic output.""")
    args = parser.parse_args()

    if args.sheet is None and args.json is None:
        print("No output specified!  Use --sheet or --json or both.")
        return 1

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
                 else (find_last_unfetched_date(rows or so_far)
                       if (args.autodate and args.update)
                       else datetime.datetime.today()))

    fetch_streak_upto(upto_date,
                      accumulator=so_far if args.json else None,
                      sheet=rows if args.sheet else None,
                      countdown=(args.number
                                      if args.number > 0
                                      else None),
                      verbose=args.verbose)

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
