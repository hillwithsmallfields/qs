#!/usr/bin/python

# Program to detect periodic payments and spot gaps in them

import argparse
import csv
import datetime
import os
import qsutils

# See notes in finconv.py for config file format

secs_per_day = 24 * 60 * 60

def finperiodic_setup(app_data, input_format):
    return ['payee'], {}

def finperiodic_row(timestamp, row, output_rows, scratch):
    timestamp = datetime.datetime.strptime(timestamp, "%Y-%m-%dT%H:%M:%S")
    payee = row['payee']
    amount = row.get('amount',
                     row.get('debits',
                             row.get('credits')))
    if payee in scratch:
        scratch[payee][timestamp] = amount
    else:
        scratch[payee] = {timestamp: amount}

def finperiodic_tidyup(columns, rows, scratch):
    for payee, transactions in scratch.iteritems():
        # print payee, transactions
        dates = sorted(transactions.keys())
        prev = dates[0]
        intervals = []
        for when in dates[1:]:
            interval = int((when-prev).total_seconds() / secs_per_day)
            if interval > 0:    # ignore further transactions on the same day
                intervals.append(interval)
            prev = when
        if len(intervals) > 1:
            counts = {}
            for interval in intervals:
                counts[interval] = counts.get(interval, 0) + 1
            print payee
            for k in sorted(counts.keys()):
                print "  ", k, counts[k]
            total = sum(counts)
            approx_weekly = sum(counts[6:8]) / total
            approx_monthly = sum(counts[26:34]) / total
            print "approx_weekly", approx_weekly
            print "approx_monthly", approx_monthly
    return None, None

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    parser.add_argument("-o", "--output")
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("input_file")

    args = parser.parse_args()

    # todo: deduce format of input file; should normally be financisto, or have similar data

    qsutils.process_fin_csv({'args': args,
                             'config': qsutils.load_config(
                                 args.verbose,
                                 None,
                                 None,
                                 qsutils.program_load_config(args),
                                 *args.config or ())},
                            finperiodic_setup,
                            finperiodic_row,
                            finperiodic_tidyup)

if __name__ == "__main__":
    main()
