#!/usr/bin/python3
# program to scrape myfitnesspal, using https://github.com/coddingtonbear/python-myfitnesspal

from decouple import config
import datetime
import json
import myfitnesspal
import random
import time

client = myfitnesspal.Client(config('MFP_USERNAME'), config('MFP_PASSWORD'))

def all_empty(data):
    for meal in data.values():
        if len(meal) > 0:
            return False
    return True

def fetch_streak_upto(when, accumulator={}, countdown=None):
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

streak = fetch_streak_upto(datetime.datetime.today(), countdown=7)

print(json.dumps({d.isoformat(): v for d,v in streak.items()}))
