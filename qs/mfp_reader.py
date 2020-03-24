#!/usr/bin/python3
# program to scrape myfitnesspal, using https://github.com/coddingtonbear/python-myfitnesspal

import argparse
from decouple import config
import datetime
import json
import myfitnesspal
import random
import time

client = None

def all_empty(data):
    for meal in data.values():
        if len(meal) > 0:
            return False
    return True

def fetch_streak_upto(when, accumulator={}, countdown=None):
    global client
    if client is None:
        client = myfitnesspal.Client(config('MFP_USERNAME'), config('MFP_PASSWORD'))
    while True:
        date_key = when.date()
        if date_key not in accumulator:
            data = client.get_date(when.year, when.month, when.day).get_as_dict()
            if all_empty(data):
                break
            accumulator[date_key] = data
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
                       help="""File to update.""")
    parser.add_argument("--latest", "-l",
                        help="""The most recent date to fetch.""")
    parser.add_argument("--preload", "-p")
    parser.add_argument("--output", "-o")
    args = parser.parse_args()

    if args.update:
        if args.preload or args.output:
            print("If --update is used, --preload and --output must not be used")
            return 1
        input_filename = args.update
        output_filename = args.update
    else:
        if args.update:
            print("If --output or --preload is used, --update must not be used.")
        input_filename = args.preload
        output_filename = args.output or "/tmp/mfp.json"

    so_far = {}
    if input_filename:
        with open(input_filename) as instream:
            so_far = {datetime.datetime.fromisoformat(k): v
                      for k, v in json.load(instream).items()}

    upto_date = (datetime.datetime.fromisoformat(args.latest)
                 if args.latest
                 else datetime.datetime.today())

    with open(output_filename, 'w') as outstream:
        json.dump({d.isoformat(): v
                   for d,v in fetch_streak_upto(
                           upto_date,
                           accumulator=so_far,
                           countdown=(args.number
                                      if args.number > 0
                                      else None)).items()},
                  outstream)

if __name__ == "__main__":
    main()
