#!/usr/bin/python3

import argparse
import datetime
import glob
import os
import shutil

import check_merged_row_dates
import finlisp
import list_completions
import mfp_reader
import qschart
import qsmerge
import qsutils

def file_newer_than_file(a, b):
    return os.path.getmtime(a) > os.path.getmtime(b)

def automatic_finances(config, charts_dir, verbose):

    main_account = os.path.expandvars("$COMMON/finances/finances.csv")
    account_archive_dir = os.path.expanduser("~/archive")
    finances_completions = os.path.expandvars("$COMMON/var/finances-completions.el")
    bank_statement_template = os.path.expanduser("~/Downloads/Transaction*.csv")
    merge_results_dir = os.path.expanduser("~/scratch/auto-merge-results")
    merge_results_file = os.path.join(merge_results_dir, "merged-with-unmatched-all.csv")

    bank_statements = glob.glob(bank_statement_template)
    bank_statements.sort(key=os.path.getmtime)
    latest_bank_statement = bank_statements[-1]

    if file_newer_than_file(latest_bank_statement, main_account):
        print("Updating from latest bank statement", latest_bank_statement)
        if os.path.isdir(merge_results_dir):
            for file in os.listdir(merge_results_dir):
                os.remove(os.path.join(merge_results_dir, file))
        else:
            os.makedirs(merge_results_dir, exist_ok=True)
        finlisp.finlisp_main(["merge-latest-statement.lisp"],
                             merge_results_dir,
                             config,
                             verbose,
                             {'incoming-statement': latest_bank_statement})
        if os.path.isfile(merge_results_file):
            shutil.copy(main_account, os.path.join(account_archive_dir, "finances-to-%s.csv" % datetime.datetime.now().isoformat()))
            shutil.copy(merge_results_file, main_account)
    else:
        print("Bank statement not newer than account file, so not updating")

    finlisp.finlisp_main(["chart-categories.lisp"],
                         charts_dir,
                         config,
                         verbose,
                         {'input-file': main_account,
                          'statements-file': os.path.expandvars("$COMMON/finances/handelsbanken/handelsbanken-full.csv"),
                          'classifiers-file': "budgetting-classes.yaml",
                          'thresholds-file': "budgetting-thresholds.yaml"})

    if file_newer_than_file(finances_completions, main_account):
        print("updating finances completions")
        list_completions.list_completions()

def automatic_physical(charts_dir):

    physical = os.path.expandvars("$COMMON/health/physical.csv")
    weight = os.path.expandvars("$COMMON/health/weight.csv")
    # TODO: temperature, blood pressure, peak flow
    phys_scratch = "/tmp/physical-tmp.csv"

    qsmerge.qsmerge(physical, [weight], None, phys_scratch)

    if check_merged_row_dates.check_merged_row_dates(phys_scratch, physical, weight):
        shutil.copy(physical, physical+"-old")
        shutil.copy(phys_scratch, physical)
        for units in ('stone', 'kilogram', 'pound'):
            qschart.qschart(physical,
                            units,  {'units': units},
                            os.path.join(charts_dir, "weight-%s.png" % units))
    else:
        print("merge of physical data produced the wrong number of rows")

def automatic_actions(charts_dir, verbose):
    configdir = os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/conf")
    conversions_dir = os.path.expandvars("$COMMON/finances")
    accounts_config = os.path.join(configdir, "accounts.yaml")
    conversions_config = os.path.join(conversions_dir, "conversions.yaml")

    config = qsutils.load_config(verbose, None, None, accounts_config, conversions_config)

    mfp_filename = os.path.expandvars("$COMMON/health/mfp-accum.csv")

    os.makedirs(charts_dir, exist_ok=True)

    automatic_finances(config, charts_dir, verbose)
    automatic_physical(charts_dir)
    mfp_reader.automatic(config, mfp_filename)

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("--charts", default="/tmp",
                        help="""Directory to write charts into.""")
    args = parser.parse_args()

    automatic_actions(args.charts,
                      args.verbose)

if __name__ == '__main__':
    main()
