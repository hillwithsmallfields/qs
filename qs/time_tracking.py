#!/usr/bin/python3

import argparse
import csv
import datetime

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input")
    args = parser.parse_args()

    days = {}
    
    with open(args.input) as instream:
        for row in csv.DictReader(instream):
            activity = row['Activity']
            if activity.lower() == 'untracked':
                continue
            start = datetime.datetime.fromisoformat(row['Datetime'])
            start_date = start.date()
            if start_date not in days:
                days[start_date] = []
            duration_parts = row['Duration'].split(':')
            duration = datetime.timedelta(hours=int(duration_parts[0]), minutes=int(duration_parts[1]))
            end = start + duration
            end_date = end.date()
            slot = (start, duration, end, activity)
            if start_date != end_date:
                print("Activity", activity, "spanned more than one day")
                days[start_date].append(slot)
                # TODO: adjust the existing slot, make a new one... also fill in days inbetween
            days[start_date].append(slot)
            
if __name__ == '__main__':
    main()
