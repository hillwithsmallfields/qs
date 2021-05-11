#!/usr/bin/python3

import argparse
import csv
import datetime
import decouple
import os
import oura

COLUMNS = ['bedtime_start', 'bedtime_end', 'onset_latency', 'duration',
           'awake', 'light', 'rem', 'restless', 'deep', 'total',
           'breath_average', 'efficiency',
           'hr_average', 'hr_lowest',
           'midpoint_at_delta', 'midpoint_time',
           'score', 'score_alignment', 'score_deep', 'score_disturbances', 'score_efficiency', 'score_latency', 'score_rem', 'score_total',
           'temperature_delta', 'temperature_deviation', 'temperature_trend_deviation',
           'hypnogram_5min']

def oura_fetch(data, start, end):
    if isinstance(start, datetime.date):
        start = start.isoformat()
    if isinstance(end, datetime.date):
        end = end.isoformat()
    client = oura.OuraClient(personal_access_token=decouple.config('OURA_PERSONAL_ACCESS_TOKEN'))
    for night in client.sleep_summary(start=start, end=end)['sleep']:
        waking = night['bedtime_end'][:10]
        night['Date'] = waking
        night['End'] = night['bedtime_end'][11:19]
        night['Start'] = night['bedtime_start'][11:19]
        for field in ['Latency',
                      'Duration',
                      'Awake',
                      'Light',
                      'Rem',
                      'Restless',
                      'Deep',
                      'Total']:
            night[field] = float(night[field.lower()]) / 3600.0
        data[waking] = {k: night.get(k) for k in COLUMNS}

def oura_read_existing(data, filename):
    with open(filename) as instream:
        for row in csv.DictReader(instream):
            data[row['bedtime_end'][:10]] = row
    return row

def oura_write(data, filename):
    with open(filename, 'w') as outstream:
        writer = csv.DictWriter(outstream, ['Date'] + COLUMNS)
        writer.writeheader()
        for date in sorted(data.keys()):
            writer.writerow(data[date])

def main():
    parser = argparse.ArgumentParser()
    beginning = parser.add_mutually_exclusive_group()
    beginning.add_argument("--start",
                           help="""The first date to fetch.""")
    beginning.add_argument("--forwards",
                           type=int,
                           default=0,
                           help="""Work forward from the latest stored date (with --update).""")
    finishing = parser.add_mutually_exclusive_group()
    finishing.add_argument("--end",
                           help="""The last date to fetch.""")
    finishing.add_argument("--backwards",
                           type=int,
                           default=0,
                           help="""Work back from the oldest stored date (with --update).""")
    parser.add_argument("-c", "--count", type=int, default=7,
                        help="""How many nights data to fetch, for --forward or --backward.""")
    result_disposition = parser.add_mutually_exclusive_group()
    result_disposition.add_argument("-o", "--output",
                                    help="""The file to write the fetched data to.""")
    result_disposition.add_argument("-u", "--update",
                                    help="""The file to update with the fetched data.""")
    args = parser.parse_args()
    if (not args.update) and (not args.output):
        args.update = os.path.expandvars("$COMMON/health/oura.csv")
    start = None
    end = None
    data = {}
    if args.update:
        oura_read_existing(data, args.update)
        if args.forwards:
            start = datetime.date.fromisoformat(max(data.keys()))
            end = (start + datetime.timedelta(days=args.forwards)).isoformat()
        if args.backwards:
            end = datetime.date.fromisoformat(min(data.keys()))
            start = (end - datetime.timedelta(days=args.backwards)).isoformat()
    if start is None:
        start = args.start or (datetime.date.today() - datetime.timedelta(days=args.count)).isoformat()
    if end is None:
        end = args.end or datetime.date.today().isoformat()
    oura_fetch(data, start, end)
    oura_write(data, args.update or args.output)

if __name__ == '__main__':
    main()
