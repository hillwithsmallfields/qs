#!/usr/bin/python3

import argparse
import csv
import datetime

NOTIME = datetime.timedelta(hours=0, minutes=0)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input")
    args = parser.parse_args()

    day_slots = {}
    activities = set()

    with open(args.input) as instream:
        for row in csv.DictReader(instream):
            activity = row['Activity']
            if activity.lower() == 'untracked':
                continue
            activities.add(activity)
            start = datetime.datetime.fromisoformat(row['Datetime'])
            start_date = start.date()
            if start_date not in day_slots:
                day_slots[start_date] = []
            duration_parts = row['Duration'].split(':')
            duration = datetime.timedelta(hours=int(duration_parts[0]), minutes=int(duration_parts[1]))
            end = start + duration
            end_date = end.date()
            slot = (start, duration, end, activity)
            day_slots[start_date].append(slot)

    day_totals = {}
    day_counts = {}
    # by activity:
    day_maxes = {}
    day_mins = {}
    activity_totals = {}
    activity_counts = {}
    activity_days = {}

    for day, slots in day_slots.items():
        totals = {}
        counts = {}
        for slot in slots:
            activity = slot[3]
            totals[activity] = totals.get(activity, NOTIME) + slot[1]
            counts[activity] = counts.get(activity, 0) + 1
            activity_totals[activity] = activity_totals.get(activity, NOTIME) + slot[1]
            activity_counts[activity] = activity_counts.get(activity, 0) + 1
            if activity not in activity_days:
                activity_days[activity] = set()
            activity_days[activity].add(day)
        day_totals[day] = totals
        day_counts[day] = counts
        for activity in activities:
            if activity in day_totals:
                if day_totals[activity] > day_maxes.get(activity, NOTIME):
                    day_maxes[activity] = day_totals[activity]
                if day_totals[activity] < day_mins.get(activity, 1440):
                    day_mins[activity] = day_totals[activity]

    for day in sorted(day_slots.keys()):
        print("date:", day)
        totals = day_totals[day]
        counts = day_counts[day]
        for activity in sorted(activities):
            if activity in totals:
                print("    ", activity, totals[activity], counts[activity], totals[activity] / counts[activity])

    for activity in sorted(activities):
        print(activity, "total", activity_totals[activity], "slot count", activity_counts[activity], "day count", len(activity_days[activity]), "min per day", day_mins[activity], "average per day", activity_totals[activity] / len(activity_days[activity]), "max per day", day_maxes[activity])

if __name__ == '__main__':
    main()
