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
import physical.oura_reader
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

def last_update_at_least_about_a_day_ago(filename):
    return ((datetime.datetime.fromtimestamp(os.path.getmtime(filename))
             + datetime.timedelta(hours=23, minutes=30))
            < datetime.datetime.now())

def update_finances(file_locations, verbose):

    config = qsutils.load_config(verbose, None, None,
                                 os.path.join(file_locations['configdir'], file_locations['accounts-config']),
                                 os.path.join(file_locations['conversions-dir'], file_locations['conversions-config']))

    main_account = file_locations['main-account']
    merge_results_dir = file_locations['merge-results-dir']

    latest_bank_statement = latest_file_matching(file_locations['bank-statement-template'])

    if latest_bank_statement and file_newer_than_file(latest_bank_statement, main_account):
        if os.path.isdir(merge_results_dir):
            for old_file in os.listdir(merge_results_dir):
                os.remove(os.path.join(merge_results_dir, old_file))
        else:
            os.makedirs(merge_results_dir, exist_ok=True)
        if verbose: print("Updating from latest bank statement", latest_bank_statement)
        financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "merge-latest-statement.lisp")],
                             merge_results_dir,
                             config,
                             verbose,
                             {'incoming-statement': latest_bank_statement})
        merge_results_file = os.path.join(merge_results_dir, file_locations['merge-results-file'])
        if os.path.isfile(merge_results_file):
            backup(main_account, file_locations['archive'], "finances-to-%s.csv")
            shutil.copy(merge_results_file, main_account)
    else:
        print("Bank statement not newer than account file, so not updating")

    financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "chart-categories.lisp")],
                                   file_locations['charts'],
                                   config,
                                   verbose,
                                   {'input-file': main_account,
                                    'statements-file': file_locations['accumulated-bank-statements-file'],
                                    'classifiers-file': file_locations['budgeting-classes-file'],
                                    'thresholds-file': file_locations['thresholds-file']})

    if file_newer_than_file(main_account, file_locations['finances-completions']):
        if verbose: print("updating finances completions")
        list_completions.list_completions()

def update_finances_charts(file_locations, begin_date, end_date, date_suffix, verbose):

    charts_dir = file_locations['charts']
    utils.qschart.qscharts(os.path.join(charts_dir, "by-class.csv"),
                           'finances',
                           dashboard.dashboard.CATEGORIES_OF_INTEREST,
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "by-class-%s-%%s.png" % date_suffix),
                           CHART_SIZES)

def update_physical_charts(file_locations, begin_date, end_date, date_suffix):

    charts_dir = file_locations['charts']
    physical = file_locations['physical-filename']
    mfp_filename = file_locations['mfp-filename']
    phys_scratch = "/tmp/physical-tmp.csv"
    archive_dir = file_locations['archive']

    physical_files = [file_locations['weight-filename']
                     # TODO: merge the other physical files
                    ]

    qsmerge.qsmerge(physical,
                    physical_files, None, phys_scratch)

    if utils.check_merged_row_dates.check_merged_row_dates(phys_scratch, physical, *physical_files):
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
    utils.qschart.qscharts(file_locations['oura-filename'], 'sleep',
                           ['Latency', 'Rem', 'Deep', 'Total'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "sleep-split-%s-%%s.png" % date_suffix),
                           CHART_SIZES)
    utils.qschart.qscharts(file_locations['oura-filename'], 'sleep',
                           ['Start', 'End'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "sleep-times-%s-%%s.png" % date_suffix),
                           CHART_SIZES)
    # utils.qschart.qscharts(smart_one_filename, 'peak_flow',
    #                        ['Peak flow'],
    #                        begin_date, end_date, None,
    #                        os.path.join(charts_dir, "peak-flow-%s-%%s.png" % date_suffix),
    #                        CHART_SIZES)
    utils.qschart.qscharts(file_locations['temperature-file'], 'temperature',
                           ['Temperature'],
                           begin_date, end_date, None,
                           os.path.join(charts_dir, "temperature-%s-%%s.png" % date_suffix),
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
    contacts_file = file_locations['contacts-file']
    contacts_scratch = "/tmp/contacts_scratch.csv"
    contacts_analysis = link_contacts.link_contacts_main(contacts_file, True, False, contacts_scratch)
    with open(contacts_file) as confile:
        original_lines = len(confile.readlines())
    with open(contacts_scratch) as conscratch:
        scratch_lines = len(conscratch.readlines())
    if original_lines == scratch_lines:
        backup(contacts_file, file_locations['archive'], "contacts-%s.csv")
        shutil.copy(contacts_scratch, contacts_file)
    else:
        print("wrong number of people after linking contacts, originally", original_lines, "but now", scratch_lines)
    return contacts_analysis

def fetch_mfp(file_locations, begin_date, end_date, verbose):
    print("Fetching data from myfitnesspal.com")
    physical.mfp_reader.update_mfp(file_locations['mfp-filename'], verbose)
    print("Fetched data from myfitnesspal.com")

def fetch_oura(file_locations, begin_date, end_date, verbose):
    oura_filename = file_locations['oura-filename']
    data = {}
    physical.oura_reader.oura_read_existing(data, oura_filename)
    existing_rows = len(data)
    if begin_date is None:
        begin_date = qsutils.earliest_unfetched(data)
    physical.oura_reader.oura_fetch(data, begin_date, end_date)
    if len(data) > existing_rows:
        physical.oura_reader.oura_write(data, oura_filename)
    elif len(data) < existing_rows:
        print("Warning: sleep data has shrunk on being fetched --- not writing it")

def fetch_omron(file_locations, begin_date, end_date, verbose):
    omron_filename = file_locations['omron-filename']
    # TODO: fetch from API

def fetch_garmin(file_locations, begin_date, end_date, verbose):
    garmin_incoming_filename = file_locations['garmin-incoming-filename']
    if (os.path.isfile(garmin_incoming_filename)
        and file_newer_than_file(garmin_incoming_filename, garmin_filename)):
        garmin_filename = file_locations['garmin-filename']
        data = {}
        if os.path.isfile(garmin_filename):
            with open(garmin_filename) as instream:
                data = {row['Date']: row for row in csv.DictReader(instream)}
        original_length = len(data)
        with open(garmin_incoming_filename) as instream:
            for row in csv.reader(instream):
                header = row
                break           # just get the first row
        with open(garmin_incoming_filename) as instream:
            data.update({row['Date']: row for row in csv.DictReader(instream)})
    if len(data) > original_length:
        with open(garmin_filename, 'w') as outstream:
            writer = csv.DictWriter(outstream, header)
            writer.writeheader()
            for row in data:
                writer.writerow(row)

def fetch_travel(file_locations, begin_date, end_date, verbose):
    # TODO: fetch from Google, updating file_locations['travel-filename'] and file_locations['places-filename']
    pass

def update_travel(file_locations):
    # TODO: write travel section of QS code
    # travel_main(file_locations['travel-filename'], file_locations['places-filename'])
    # TODO: calculate distances
    pass

def updates(file_locations,
            begin_date, end_date,
            do_externals, verbose):

    os.makedirs(file_locations['charts'], exist_ok=True)
    # if end_date is None:
    #     end_date = qsutils.yesterday()

    if do_externals:
        for location_name, fetcher, archive_template in [
                ('mfp-filename', fetch_mfp, "mfp-to-%s.csv"),
                ('travel-filename', fetch_travel, "travel-to-%s.csv"),
                ('oura-filename', fetch_oura, "oura-to-%s.csv"),
                ('omron-filename', fetch_omron, "omron-to-%s.csv"),
                ('garmin-filename', fetch_garmin, "garmin-to-%s.csv")
                ]:
            filename = file_locations[location_name]
            if last_update_at_least_about_a_day_ago(filename):
                backup(filename, file_locations['archive'], archive_template)
                fetcher(file_locations, begin_date, end_date, verbose)

    update_finances(file_locations, verbose)
    contacts_analysis = update_contacts(file_locations)
    update_travel(file_locations)

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
        update_finances_charts(file_locations, begin, end_date, date_suffix, verbose)
        update_physical_charts(file_locations, begin, end_date, date_suffix)

    dashboard.dashboard.write_dashboard_page(file_locations,
                                             contacts_analysis,
                                             details_background_color=shading)
    update_startpage(file_locations)

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
        # finance
        'accounts-config': "accounts.yaml",
        'accumulated-bank-statements-file': "$COMMON/finances/handelsbanken/handelsbanken-full.csv",
        'bank-statement-template': "~/Downloads/Transaction*.csv",
        'budgeting-classes-file': "budgetting-classes.yaml",
        'configdir': "~/open-projects/github.com/hillwithsmallfields/qs/conf",
        'conversions-config': "conversions.yaml",
        'conversions-dir': "$COMMON/finances",
        'finances-completions': "$COMMON/var/finances-completions.el",
        'main-account': "$COMMON/finances/finances.csv",
        'merge-results-dir': "~/scratch/auto-merge-results",
        'merge-results-file': "merged-with-unmatched-all.csv",
        'thresholds-file': "budgetting-thresholds.yaml",

        # inventory
        'books-file': "$COMMON/org/books.csv",
        'inventory-file': "$COMMON/org/inventory.csv",
        'project-parts-file': "$COMMON/org/project-parts.csv",
        'stock-file': "$COMMON/org/stock.csv",
        'storage-file': "$COMMON/org/storage.csv",

        # physical
        'garmin-filename': "$COMMON/health/garmin.csv",
        'garmin-incoming-filename': "~/Downloads/Activities*.csv",
        'mfp-filename': "$COMMON/health/mfp-accum.csv",
        'omron-filename': "$COMMON/health/blood-pressure.csv",
        'oura-filename': "$COMMON/health/sleep.csv",
        'physical-filename': "$COMMON/health/physical.csv",
        'weight-filename': "$COMMON/health/weight.csv",
        'temperature-file': "$COMMON/health/temperature.csv",

        # start page
        'start-page-generator': 'make_link_table.py',
        'startpage': "~/private_html/startpage.html",
        'startpage-source': "$COMMON/org/startpage.yaml",
        'startpage-style': "$COMMON/org/startpage.css",

        # travel
        'travel-filename': "$COMMON/travel/travel.csv",
        'places-filename': "$COMMON/travel/places/places.csv",

        # contacts
        'contacts-file': "$COMMON/org/contacts.csv",

        'charts': args.charts,
        'default-timetable': "timetable.csv",
        'projects-dir': "~/open-projects/github.com",
        'projects-user': "hillwithsmallfields",
        'reflections-dir': os.path.expandvars("$COMMON/texts/reflection"),
        'timetables-dir': "$COMMON/timetables",
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
