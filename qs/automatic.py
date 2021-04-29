#!/usr/bin/python3

import argparse
import os
import shutil

import check_merged_row_dates
import qschart
import qsmerge

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--charts", default="/tmp",
                        help="""Directory to write charts into.""")
    args = parser.parse_args()

    physical = os.path.expandvars("$COMMON/health/physical.csv")
    weight = os.path.expandvars("$COMMON/health/weight.csv")
    scratch = "/tmp/physical-tmp.csv"
    qsmerge.qsmerge(physical, [weight], None, scratch)

    if check_merged_row_dates.check_merged_row_dates(scratch, physical, weight):
        shutil.copy(physical, physical+"-old")
        shutil.copy(scratch, physical)

    for units in ("Stone", "Kg"):
        qschart.qschart("~/common/health/physical.csv",
                        units,  {'units': units},
                        os.path.join(args.charts, "weight-%s.png" % units))

if __name__ == '__main__':
    main()
