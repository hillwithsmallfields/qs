#!/usr/bin/python3

import argparse
import datetime
import glob
import os
import re
import shutil
import sys
import yaml

import numpy as np

sys.path.append(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import utils.check_merged_row_dates
import dashboard.dashboard
import financial.finlisp
import list_completions
import physical.mfp_reader
import utils.qschart
import qsmerge
import qsutils

my_projects = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
print("update.py sees my_projects as", my_projects)
sys.path.append(os.path.join(my_projects, "coimealta/contacts"))
import link_contacts

CHART_SIZES = {'thumb': {'figsize': (3,2)},
               'small': {'figsize': (5,4)},
               'large': {'figsize': (11,8)}}

def file_newer_than_file(a, b):
    return os.path.getmtime(a) > os.path.getmtime(b)

def backup(filename, archive_dir, template):
    os.system("gzip --to-stdout %s > %s" % (
        filename,
        os.path.join(archive_dir,
                     (template % datetime.datetime.now().isoformat()) + ".gz")))

def latest_file_matching(template):
    files = glob.glob(template)
    print("looking for files matching", template, "and got", files)
    return files and sorted(files, key=os.path.getmtime)[-1]

def update_finances(config, charts_dir, archive_dir, verbose):

    main_account = os.path.expandvars("$COMMON/finances/finances.csv")
    finances_completions = os.path.expandvars("$COMMON/var/finances-completions.el")
    bank_statement_template = os.path.expanduser("~/Downloads/Transaction*.csv")
    merge_results_dir = os.path.expanduser("~/scratch/auto-merge-results")
    merge_results_file = os.path.join(merge_results_dir, "merged-with-unmatched-all.csv")

    latest_bank_statement = latest_file_matching(bank_statement_template)

    if latest_bank_statement and file_newer_than_file(latest_bank_statement, main_account):
        print("Updating from latest bank statement", latest_bank_statement)
        if os.path.isdir(merge_results_dir):
            for file in os.listdir(merge_results_dir):
                os.remove(os.path.join(merge_results_dir, file))
        else:
            os.makedirs(merge_results_dir, exist_ok=True)
        financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "merge-latest-statement.lisp")],
                             merge_results_dir,
                             config,
                             verbose,
                             {'incoming-statement': latest_bank_statement})
        if os.path.isfile(merge_results_file):
            backup(main_account, archive_dir, "finances-to-%s.csv")
            shutil.copy(merge_results_file, main_account)
    else:
        print("Bank statement not newer than account file, so not updating")

    financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "chart-categories.lisp")],
                                   charts_dir,
                                   config,
                                   verbose,
                                   {'input-file': main_account,
                                    'statements-file': os.path.expandvars("$COMMON/finances/handelsbanken/handelsbanken-full.csv"),
                                    'classifiers-file': "budgetting-classes.yaml",
                                    'thresholds-file': "budgetting-thresholds.yaml"})

    if file_newer_than_file(finances_completions, main_account):
        if verbose: print("updating finances completions")
        list_completions.list_completions()

def update_finances_charts(config, charts_dir, begin_date, end_date, date_suffix, verbose):

    utils.qschart.qscharts(os.path.join(charts_dir, "by-class.csv"),
                           'finances',
                           dashboard.dashboard.CATEGORIES_OF_INTEREST,
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "by-class-%s-%%s.png" % date_suffix),
                           CHART_SIZES)

def update_physical(charts_dir, begin_date, end_date, date_suffix, archive_dir):

    physical = os.path.expandvars("$COMMON/health/physical.csv")
    weight = os.path.expandvars("$COMMON/health/weight.csv")
    mfp_filename = os.path.expandvars("$COMMON/health/mfp-accum.csv")
    # TODO: temperature, blood pressure, peak flow
    phys_scratch = "/tmp/physical-tmp.csv"

    qsmerge.qsmerge(physical, [weight], None, phys_scratch)

    if utils.check_merged_row_dates.check_merged_row_dates(phys_scratch, physical, weight):
        backup(physical, archive_dir, "physical-to-%s.csv")
        shutil.copy(phys_scratch, physical)
        for units in ('stone', 'kilogram', 'pound'):
            utils.qschart.qscharts(physical,
                                   'weight',
                                   [units],
                                   begin_date, end_date, None,
                                   os.path.join(charts_dir, "weight-%s-%s-%%s.png" % (units, date_suffix)),
                                   CHART_SIZES)
    else:
        print("merge of physical data produced the wrong number of rows")

    utils.qschart.qscharts(mfp_filename,
                           'calories',
                           ['calories', 'breakfast', 'lunch', 'dinner', 'snacks'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "meal_calories-%s-%%s.png" % date_suffix),
                           CHART_SIZES)
    utils.qschart.qscharts(mfp_filename,
                           'food_groups',
                           ['carbohydrates', 'fat', 'protein', 'sugar'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "origin_calories-%s-%%s.png" % date_suffix),
                           CHART_SIZES)

def update_startpage():
    startpage = os.path.expanduser("~/private_html/startpage.html")
    startpage_source = os.path.expanduser("~/common/org/startpage.yaml")
    startpage_style = os.path.expanduser("~/common/org/startpage.css")
    if (os.path.getmtime(startpage_source) > os.path.getmtime(startpage)
        or os.path.getmtime(startpage_style) > os.path.getmtime(startpage)):
        os.system("make_link_table.py --output %s --stylesheet %s %s"
                  % (startpage, startpage_style, startpage_source))

def update_contacts(charts_dir, archive_dir):
    contacts_file = os.path.expandvars("$COMMON/org/contacts.csv")
    contacts_scratch = "/tmp/contacts_scratch.csv"
    link_contacts.link_contacts_main(contacts_file, False, False, contacts_scratch)
    with open(contacts_file) as confile:
        original_lines = len(confile.readlines())
    with open(contacts_scratch) as conscratch:
        scratch_lines = len(conscratch.readlines())
    if original_lines == scratch_lines:
        backup(contacts_file, archive_dir, "contacts-%s.csv")
        shutil.copy(contacts_scratch, contacts_file)
    else:
        print("wrong number of people after linking contacts, originally", original_lines, "but now", scratch_lines)

def make_tarball(tarball, parent_directory, of_directory):
    if not os.path.isfile(tarball):
        command = "tar cz -C %s %s > %s" % (parent_directory, of_directory, tarball)
        print("backup command is", command)
        os.system(command)

def update_backups():
    common_backups = os.path.expanduser("~/common-backups")
    daily_backup_template = "org-%s.tgz"
    weekly_backup_template = "common-%s.tgz"
    today = datetime.date.today()
    make_tarball(os.path.join(common_backups, daily_backup_template % today.isoformat()),
                 os.path.expandvars("$COMMON"),
                 "org")
    weekly_backup_day = 2    # Wednesday, probably the least likely day to be on holiday and not using the computer
    if today.weekday() == weekly_backup_day:
        make_tarball(os.path.join(common_backups, weekly_backup_template % today.isoformat()),
                     os.path.expandvars("$HOME"), "common")
    if today.day == 1:
        backup_isos_directory = os.path.expanduser("~/isos/backups")
        monthly_backup_name = os.path.join(backup_isos_directory, "backup-%s.iso" % today.isoformat())
        if not os.path.isfile(monthly_backup_name):
            # make_tarball("/tmp/music.tgz", os.path.expandvars("$HOME"), "Music")
            make_tarball("/tmp/github.tgz", os.path.expanduser("~/open-projects/github.com"), "hillwithsmallfields")
            files_to_backup = [
                latest_file_matching(os.path.join(common_backups, daily_backup_template % "*")),
                latest_file_matching(os.path.join(common_backups, weekly_backup_template % "*")),
                # too large for genisoimage:
                # "/tmp/music.tgz",
                "/tmp/github.tgz"]
            # look for the output of https://github.com/hillwithsmallfields/JCGS-scripts/blob/master/backup-confidential
            confidential_backup = latest_file_matching("/tmp/personal-*.tgz.gpg")
            if confidential_backup:
                files_to_backup.append(confidential_backup)
                digest = confidential_backup.replace('gpg', 'sha256sum')
                if os.path.isfile(digest):
                    files_to_backup.append(digest)
                sig = digest + ".sig"
                if os.path.isfile(sig):
                    files_to_backup.append(sig)
            print("Time to take a monthly backup of", files_to_backup, "into", monthly_backup_name)
            os.system("genisoimage -o %s %s" % (monthly_backup_name, " ".join(files_to_backup)))
            print("made backup in", monthly_backup_name)

def updates(charts_dir,
            begin_date, end_date,
            do_externals, verbose):
    archive_dir = os.path.expanduser("~/archive")
    configdir = os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/conf")
    conversions_dir = os.path.expandvars("$COMMON/finances")
    accounts_config = os.path.join(configdir, "accounts.yaml")
    conversions_config = os.path.join(conversions_dir, "conversions.yaml")

    config = qsutils.load_config(verbose, None, None, accounts_config, conversions_config)

    update_finances(config, charts_dir, archive_dir, verbose)
    update_contacts(charts_dir, archive_dir)
    if do_externals:
        mfp_filename = os.path.expandvars("$COMMON/health/mfp-accum.csv")
        if ((datetime.datetime.fromtimestamp(os.path.getmtime(mfp_filename))
             + datetime.timedelta(hours=23, minutes=30))
            < datetime.datetime.now()):
            print("Fetching data from myfitnesspal.com")
            physical.mfp_reader.update_mfp(config, mfp_filename, verbose)
            print("Fetched data from myfitnesspal.com")
        else:
            print("myfitnesspal.com data fetched within the past day or so, so not doing again yet")

    os.makedirs(charts_dir, exist_ok=True)

    today = datetime.date.today()
    _, background_colour = dashboard.dashboard.dashboard_page_colours()
    for param_set in CHART_SIZES.values():
        param_set['facecolor'] = background_colour

    periods = {'all_time': datetime.date(year=1973, month=1, day=1),
               'past_week': qsutils.back_from(today, None, None, 7),
               'past_month': qsutils.back_from(today, None, 1, None),
               'past_quarter': qsutils.back_from(today, None, 3, None),
               'past_year': qsutils.back_from(today, 1, None, None)}
    for date_suffix, begin in ({'custom': begin_date}
                               if begin_date
                               else periods).items():
        begin = np.datetime64(datetime.datetime.combine(begin, datetime.time())) # .timestamp()
        update_finances_charts(config, charts_dir, begin, end_date, date_suffix, verbose)
        update_physical(charts_dir, begin, end_date, date_suffix, archive_dir)
        # TODO: get oura.com data
        # TODO: get garmin data

    dashboard.dashboard.write_dashboard_page(config, charts_dir)
    update_startpage()
    update_backups()

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/private_html/dashboard"),
                        help="""Directory to write charts into.""")
    parser.add_argument("--begin",
                        help="""Earliest date to chart.""")
    parser.add_argument("--end",
                        help="""Latest date to chart.""")
    parser.add_argument("--no-externals", action='store_true',
                        help="""Don't pester external servers""")
    args = parser.parse_args()

    updates(args.charts,
            args.begin,
            args.end,
            not args.no_externals,
            args.verbose)

if __name__ == '__main__':
    main()
