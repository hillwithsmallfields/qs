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

# other parts of this project group:
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
sys.path.append(os.path.join(my_projects, "coimealta/contacts"))
import link_contacts # https://github.com/hillwithsmallfields/coimealta/blob/master/contacts/link_contacts.py

CHART_SIZES = {'small': {'figsize': (5,4)},
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

def update_finances(config, file_locations, verbose):

    charts_dir = file_locations['charts']
    archive_dir = file_locations['archive']

    main_account = file_locations['main-account']
    finances_completions = file_locations['finances-completions']
    bank_statement_template = file_locations['bank-statement-template']
    merge_results_dir = file_locations['merge-results-dir']
    merge_results_file = os.path.join(merge_results_dir, file_locations['merge-results-file'])

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
                                    'statements-file': file_locations['accumulated-bank-statements-file'],
                                    'classifiers-file': file_locations['budgeting-classes-file'],
                                    'thresholds-file': file_locations['thresholds-file']})

    if file_newer_than_file(finances_completions, main_account):
        if verbose: print("updating finances completions")
        list_completions.list_completions()

def update_finances_charts(config, file_locations, begin_date, end_date, date_suffix, verbose):

    charts_dir = file_locations['charts']
    utils.qschart.qscharts(os.path.join(charts_dir, "by-class.csv"),
                           'finances',
                           dashboard.dashboard.CATEGORIES_OF_INTEREST,
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "by-class-%s-%%s.png" % date_suffix),
                           CHART_SIZES)

def update_physical(file_locations, begin_date, end_date, date_suffix):

    charts_dir = file_locations['charts']
    physical = file_locations['physical-filename']
    weight = file_locations['weight-filename']
    mfp_filename = file_locations['mfp-filename']
    oura_filename = file_locations['oura-filename']
    # TODO: temperature, blood pressure, peak flow
    phys_scratch = "/tmp/physical-tmp.csv"
    archive_dir = file_locations['archive']

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
                           'calories', ['calories'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "total_calories-%s-%%s.png" % date_suffix),
                           CHART_SIZES)
    utils.qschart.qscharts(mfp_filename, 'meals',
                           ['breakfast', 'lunch', 'dinner', 'snacks'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "meal_calories-%s-%%s.png" % date_suffix),
                           CHART_SIZES)
    utils.qschart.qscharts(mfp_filename, 'food_groups',
                           ['carbohydrates', 'fat', 'protein', 'sugar'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "origin_calories-%s-%%s.png" % date_suffix),
                           CHART_SIZES)
    utils.qschart.qscharts(oura_filename, 'sleep',
                           [
                               # TODO: chart the start and end times
                               # TODO: correlation between start time and various measures of sleep quality
                               # 'Start', 'End',
                               'Latency', 'Rem', 'Deep', 'Total'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "sleep-%s-%%s.png" % date_suffix),
                           CHART_SIZES)
    # utils.qschart.qscharts(omron_filename, 'blood_pressure',
    #                        ['Systolic', 'Diastolic', 'Heart rate'],
    #                        begin_date, end_date, None,
    #                        os.path.join(charts_dir, "blood-pressure-%s-%%s.png" % date_suffix),
    #                        CHART_SIZES)
    # utils.qschart.qscharts(garmin_filename, 'exercise',
    #                        ['Cycling', 'Running'],
    #                        begin_date, end_date, None,
    #                        os.path.join(charts_dir, "exercise-%s-%%s.png" % date_suffix),
    #                        CHART_SIZES))

def update_startpage(file_locations):
    startpage = file_locations['startpage']
    startpage_source = file_locations['startpage-source']
    startpage_style = file_locations['startpage-style']
    if (os.path.getmtime(startpage_source) > os.path.getmtime(startpage)
        or os.path.getmtime(startpage_style) > os.path.getmtime(startpage)):
        os.system("%s --output %s --stylesheet %s %s"
                  % (file_locations['start-page-generator'], startpage, startpage_style, startpage_source))

def update_contacts(file_locations):
    archive_dir = file_locations['archive']
    contacts_file = file_locations['contacts-file']
    contacts_scratch = "/tmp/contacts_scratch.csv"
    contacts_analysis = link_contacts.link_contacts_main(contacts_file, True, False, contacts_scratch)
    with open(contacts_file) as confile:
        original_lines = len(confile.readlines())
    with open(contacts_scratch) as conscratch:
        scratch_lines = len(conscratch.readlines())
    if original_lines == scratch_lines:
        backup(contacts_file, archive_dir, "contacts-%s.csv")
        shutil.copy(contacts_scratch, contacts_file)
    else:
        print("wrong number of people after linking contacts, originally", original_lines, "but now", scratch_lines)
    return contacts_analysis

def update_travel(do_externals):
    # TODO: write travel section of QS code
    if do_externals:
        # TODO: fetch from Google
        pass
    # TODO: calculate distances
    pass

def make_tarball(tarball, parent_directory, of_directory):
    if not os.path.isfile(tarball):
        command = "tar cz -C %s %s > %s" % (parent_directory, of_directory, tarball)
        print("backup command is", command)
        os.system(command)

def update_backups(file_locations):
    common_backups = file_locations['common-backups']
    daily_backup_template = file_locations['daily-backup-template']
    weekly_backup_template = file_locations['weekly-backup-template']
    today = datetime.date.today()
    make_tarball(os.path.join(common_backups, daily_backup_template % today.isoformat()),
                 os.path.expandvars("$COMMON"),
                 "org")
    weekly_backup_day = 2    # Wednesday, probably the least likely day to be on holiday and not using the computer
    if today.weekday() == weekly_backup_day:
        make_tarball(os.path.join(common_backups, weekly_backup_template % today.isoformat()),
                     os.path.expandvars("$HOME"), "common")
    if today.day == 1:
        backup_isos_directory = file_locations['backup_isos_directory']
        monthly_backup_name = os.path.join(backup_isos_directory, file_locations['backup-iso-format'] % today.isoformat())
        if not os.path.isfile(monthly_backup_name):
            # make_tarball("/tmp/music.tgz", os.path.expandvars("$HOME"), "Music")
            make_tarball("/tmp/github.tgz",
                         file_locations['projects-dir'],
                         file_locations['projects-user'])
            files_to_backup = [
                latest_file_matching(os.path.join(common_backups, daily_backup_template % "*")),
                latest_file_matching(os.path.join(common_backups, weekly_backup_template % "*")),
                # too large for genisoimage:
                # "/tmp/music.tgz",
                "/tmp/github.tgz"]
            # prepare a backup of my encrypted partition, if mounted
            if os.path.isdir(os.path.expandvars("/mnt/crypted/$USER")):
                os.system("backup-confidential")
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

def updates(file_locations,
            begin_date, end_date,
            do_externals, verbose):
    charts_dir = file_locations['charts']
    archive_dir = file_locations['archive']
    configdir = file_locations['configdir']
    conversions_dir = file_locations['conversions-dir']
    accounts_config = os.path.join(configdir, file_locations['accounts-config'])
    conversions_config = os.path.join(conversions_dir,
                                      file_locations['conversions-config'])

    config = qsutils.load_config(verbose, None, None, accounts_config, conversions_config)

    update_finances(config, file_locations, verbose)
    contacts_analysis = update_contacts(file_locations)
    update_travel(do_externals)
    if do_externals:
        mfp_filename = file_locations['mfp-filename']
        if ((datetime.datetime.fromtimestamp(os.path.getmtime(mfp_filename))
             + datetime.timedelta(hours=23, minutes=30))
            < datetime.datetime.now()):
            print("Fetching data from myfitnesspal.com")
            physical.mfp_reader.update_mfp(config, mfp_filename, verbose)
            print("Fetched data from myfitnesspal.com")
        else:
            print("myfitnesspal.com data fetched within the past day or so, so not doing again yet")
        # TODO: fetch Oura data
        # TODO: fetch Omron data
        # TODO: fetch Garmin data

    os.makedirs(charts_dir, exist_ok=True)

    today = datetime.date.today()
    text_colour, background_colour, shading = dashboard.dashboard.dashboard_page_colours()
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
        update_finances_charts(config, file_locations, begin, end_date, date_suffix, verbose)
        update_physical(file_locations, begin, end_date, date_suffix)

    dashboard.dashboard.write_dashboard_page(config, file_locations, contacts_analysis, details_background_color=shading)
    update_startpage(file_locations)
    update_backups(file_locations)

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

    file_locations = {
        'accounts-config': "accounts.yaml",
        'accumulated-bank-statements-file': "$COMMON/finances/handelsbanken/handelsbanken-full.csv",
        'archive': "~/archive",
        'backup-iso-format': "backup-%s.iso",
        'backup_isos_directory': "~/isos/backups",
        'bank-statement-template': "~/Downloads/Transaction*.csv",
        'budgeting-classes-file': "budgetting-classes.yaml",
        'charts': args.charts,
        'common-backups': "~/common-backups",
        'configdir': "~/open-projects/github.com/hillwithsmallfields/qs/conf",
        'contacts-file': "$COMMON/org/contacts.csv",
        'conversions-config': "conversions.yaml",
        'conversions-dir': "$COMMON/finances",
        'daily-backup-template': "org-%s.tgz",
        'default-timetable': "timetable.csv",
        'finances-completions': "$COMMON/var/finances-completions.el",
        'main-account': "$COMMON/finances/finances.csv",
        'merge-results-dir': "~/scratch/auto-merge-results",
        'merge-results-file': "merged-with-unmatched-all.csv",
        'mfp-filename': "$COMMON/health/mfp-accum.csv",
        'oura-filename': "$COMMON/health/sleep.csv",
        'physical-filename': "$COMMON/health/physical.csv",
        'projects-dir': "~/open-projects/github.com",
        'projects-user': "hillwithsmallfields",
        'reflections-dir': os.path.expandvars("$COMMON/texts/reflection"),
        'start-page-generator': 'make_link_table.py',
        'startpage': "~/private_html/startpage.html",
        'startpage-source': "~/common/org/startpage.yaml",
        'startpage-style': "~/common/org/startpage.css",
        'thresholds-file': "budgetting-thresholds.yaml",
        'timetables-dir': "$COMMON/timetables",
        'weekly-backup-template': "common-%s.tgz",
        'weight-filename': "$COMMON/health/weight.csv",
    }

    file_locations = {k: os.path.expanduser(os.path.expandvars(v))
                      for k, v in file_locations.items()}

    updates(file_locations,
            args.begin,
            args.end,
            not args.no_externals,
            args.verbose)

if __name__ == '__main__':
    main()
