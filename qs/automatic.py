#!/usr/bin/python3

import argparse
import csv
import datetime
import glob
import os
import shutil
import sys
import yaml

import check_merged_row_dates
import classify
import finlisp
import list_completions
import mfp_reader
import qschart
import qsmerge
import qsutils

my_projects = os.path.dirname(os.path.dirname(sys.path[0]))
sys.path.append(os.path.join(my_projects, "makers", "untemplate"))
import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

sys.path.append(os.path.join(my_projects, "coimealta"))

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

DASHBOARD_STYLESHEET = """
<style>
td.ok {
    background-color: green;
}
td.overspent {
    background-color: red;
}
</style>
"""

def make_remaining_cell(thresholds, spent_this_month, coi):
    available = float(thresholds.get(coi, 0))
    spent_string = spent_this_month.get(coi, "")
    spent = 0 if spent_string == "" else float(spent_string)
    return T.td(class_="ok" if spent <= available else "overspent")[str(available + spent)]

def construct_dashboard_page(config, charts_dir):
    thresholds = classify.read_thresholds(config, "budgetting-thresholds.yaml")
    print("thresholds are", thresholds)
    with open(os.path.join(charts_dir, "by-class-this-year.csv")) as spent_stream:
        spent = [row for row in csv.DictReader(spent_stream)]
    spent_this_month = spent[-1]
    return [T.body[
        T.h1["My dashboard"],
        T.h2["Weight"],
        T.img(src="weight-stone.png"),
        T.h2["Spending"],
        T.img(src="by-class.png"),
        T.h3["Monthly budgets"],
        T.table[T.tr[[T.th[""]] + [T.th[coi] for coi in CATEGORIES_OF_INTEREST]],
                T.tr[[T.th["Thresholds"]] + [T.td[str(thresholds[coi])] for coi in CATEGORIES_OF_INTEREST]],
                T.tr[[T.th["Spent"]] + [T.td[str(spent_this_month[coi])] for coi in CATEGORIES_OF_INTEREST]],
                T.tr[[T.th["Remaining"]] + [make_remaining_cell(thresholds, spent_this_month, coi) for coi in CATEGORIES_OF_INTEREST]]

        ],
        T.h2["Upcoming birthdays"],
        # TODO: birthday list from Coimealta
        T.h2["Food to use up in fridge"],
        # TODO: perishables from Coimealta
        # TODO: actions list from org-mode
        # TODO: shopping list from org-mode
    ]]

def page_text(page_contents, style_text, script_text):
    return untemplate.Serializer(untemplate.examples_vmap, 'utf-8').serialize(
        untemplate.HTML5Doc([untemplate.safe_unicode(style_text
                                                     + script_text),
                             page_contents]))

def write_dashboard_page(config, charts_dir):
    with open(os.path.join(charts_dir, "index.html"), 'w') as page_stream:
        page_stream.write(page_text(construct_dashboard_page(config, charts_dir),
                                    DASHBOARD_STYLESHEET, ""))

def file_newer_than_file(a, b):
    return os.path.getmtime(a) > os.path.getmtime(b)

def backup(filename, archive_dir, template):
    os.system("gzip --to-stdout %s > %s" % (
        filename,
        os.path.join(archive_dir,
                     (template % datetime.datetime.now().isoformat()) + ".gz")))

def automatic_finances(config, charts_dir, begin_date, end_date, archive_dir, verbose):

    main_account = os.path.expandvars("$COMMON/finances/finances.csv")
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
            backup(main_account, archive_dir, "finances-to-%s.csv")
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

    today = datetime.date.today()
    for begin, chart_filename in ([(begin_date, "by-class.png")]
                                  if begin_date
                                  else [(None, "by-class.png"),
                                        (back_from(today, None, None, 7), "by-class-past-week.png"),
                                        (back_from(today, None, 1, None), "by-class-past-month.png"),
                                        (back_from(today, None, 3, None), "by-class-past-quarter.png"),
                                        (back_from(today, 1, None, None), "by-class-past-year.png")]):
        qschart.qschart(os.path.join(charts_dir, "by-class.csv"),
                        'finances',
                        CATEGORIES_OF_INTEREST,
                        begin, end_date, None,
                        os.path.join(charts_dir, chart_filename))

    if file_newer_than_file(finances_completions, main_account):
        if verbose: print("updating finances completions")
        list_completions.list_completions()

def back_from(when, years_back, months_back, days_back):
    if months_back and months_back >= 12:
        years_back = (years_back or 0) + months_back / 12
        months_back %= 12
    if years_back:
        when = when.replace(year=when.year - years_back)
    if months_back:
        if months_back >= when.month:
            when = when.replace(year=when.year - 1, month=12 + when.month - months_back)
        else:
            when = when.replace(month=when.month - months_back)
    if days_back:
        when = when - datetime.timedelta(days=days_back)
    return datetime.datetime.combine(when, datetime.time())

def automatic_physical(charts_dir, begin_date, end_date, archive_dir):

    physical = os.path.expandvars("$COMMON/health/physical.csv")
    weight = os.path.expandvars("$COMMON/health/weight.csv")
    # TODO: temperature, blood pressure, peak flow
    phys_scratch = "/tmp/physical-tmp.csv"

    qsmerge.qsmerge(physical, [weight], None, phys_scratch)

    if check_merged_row_dates.check_merged_row_dates(phys_scratch, physical, weight):
        backup(physical, archive_dir, "physical-to-%s.csv")
        shutil.copy(phys_scratch, physical)
        today = datetime.date.today()
        for begin, template in ([(begin_date, "weight-%s.png")]
                                if begin_date
                                else [(None, "weight-%s.png"),
                                      (back_from(today, None, None, 7), "weight-past-week-%s.png"),
                                      (back_from(today, None, 1, None), "weight-past-month-%s.png"),
                                      (back_from(today, None, 3, None), "weight-past-quarter-%s.png"),
                                      (back_from(today, 1, None, None), "weight-past-year-%s.png")]):
            for units in ('stone', 'kilogram', 'pound'):
                qschart.qschart(physical,
                                'weight',
                                [units],
                                begin, end_date, None,
                                os.path.join(charts_dir, template % units))
    else:
        print("merge of physical data produced the wrong number of rows")

def automatic_actions(charts_dir,
                      begin_date, end_date,
                      do_externals, verbose):
    archive_dir = os.path.expanduser("~/archive")
    configdir = os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/conf")
    conversions_dir = os.path.expandvars("$COMMON/finances")
    accounts_config = os.path.join(configdir, "accounts.yaml")
    conversions_config = os.path.join(conversions_dir, "conversions.yaml")

    config = qsutils.load_config(verbose, None, None, accounts_config, conversions_config)

    mfp_filename = os.path.expandvars("$COMMON/health/mfp-accum.csv")

    os.makedirs(charts_dir, exist_ok=True)

    automatic_finances(config, charts_dir, begin_date, end_date, archive_dir, verbose)
    automatic_physical(charts_dir, begin_date, end_date, archive_dir)
    if do_externals:
        mfp_reader.automatic(config, mfp_filename, verbose)

    write_dashboard_page(config, charts_dir)

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/public_html/dashboard"),
                        help="""Directory to write charts into.""")
    parser.add_argument("--begin",
                        help="""Earliest date to chart.""")
    parser.add_argument("--end",
                        help="""Latest date to chart.""")
    parser.add_argument("--no-externals", action='store_true',
                        help="""Don't pester external servers""")
    args = parser.parse_args()

    automatic_actions(args.charts,
                      args.begin,
                      args.end,
                      not args.no_externals,
                      args.verbose)

if __name__ == '__main__':
    main()
