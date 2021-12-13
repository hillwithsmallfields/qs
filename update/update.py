#!/usr/bin/env python3

import argparse
import csv
import datetime
import decouple
import glob
import io
import json
import os
import pyowm
import re
import requests
import shutil
import sys
import yaml

# other parts of this project group:
sys.path.append(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import dashboard.dashboard
import qsutils.check_merged_row_dates
import financial.finlisp
import list_completions
import physical.mfp_reader
import physical.oura_reader
import qsutils.trim_csv
import qsutils.qsmerge
import qsutils

my_projects = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
sys.path.append(os.path.join(my_projects, "coimealta/contacts"))
import link_contacts # https://github.com/hillwithsmallfields/coimealta/blob/master/contacts/link_contacts.py

sys.path.append(os.path.join(my_projects, "noticeboard"))

import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

CONFIGURATION = {}

def CONF(*keys):
    return lifehacking_config.lookup(CONFIGURATION, *keys)

def FILECONF(*keys):
    return os.path.expanduser(os.path.expandvars(CONF(*keys)))

CHART_SIZES = {'small': {'figsize': (5,4)},
               'large': {'figsize': (11,8)}}

def file_newer_than_file(a, b):
    return os.path.getmtime(a) > os.path.getmtime(b)

def backup(filename, archive_dir, template):
    if os.path.isfile(filename):
        print("backup", filename, archive_dir, template)
        os.system("gzip --to-stdout %s > %s" % (
            filename,
            os.path.join(archive_dir,
                         (template % datetime.datetime.now().isoformat()) + ".gz")))

def latest_file_matching(template):
    files = glob.glob(template)
    return files and sorted(files, key=os.path.getmtime)[-1]

def last_update_at_least_about_a_day_ago(filename):
    return ((not os.path.isfile(filename))
            or ((datetime.datetime.fromtimestamp(os.path.getmtime(filename))
                 + datetime.timedelta(hours=23, minutes=30))
                < datetime.datetime.now()))

def update_finances(verbose):

    """Merge new transactions from my bank statement (if I've saved a new bank statement file) and prepare CSV
    files for making into charts, and HTML for incorporating into the dashboard page."""

    config = qsutils.qsutils.load_config(verbose, None, None,
                                         os.path.join(FILECONF('finance', 'configdir'), CONF('finance', 'accounts-config')),
                                         os.path.join(FILECONF('finance', 'conversions-dir'), CONF('finance', 'conversions-config')))

    main_account = FILECONF('finance', 'main-account')
    merge_results_dir = FILECONF('finance', 'merge-results-dir')

    latest_bank_statement = latest_file_matching(FILECONF('finance', 'bank-statement-template'))

    if latest_bank_statement and file_newer_than_file(latest_bank_statement, main_account):
        qsutils.qsutils.ensure_directory_present_and_empty(merge_results_dir)
        if verbose: print("Updating from latest bank statement", latest_bank_statement)
        financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "merge-latest-statement.lisp")],
                                       merge_results_dir,
                                       config,
                                       verbose,
                                       {'incoming-statement': latest_bank_statement,
                                        'verbose': verbose})
        merge_results_file = os.path.join(merge_results_dir, CONF('finance', 'merge-results-file'))
        if os.path.isfile(merge_results_file):
            backup(main_account, FILECONF('backups', 'archive'), "finances-to-%s.csv")
            shutil.copy(merge_results_file, main_account)
            if verbose: print("Merged bank statement into account file")
    else:
        print("Bank statement not newer than account file, so not updating")

    print("calling charter on", main_account, "with merge results in", merge_results_dir)

    financial.finlisp.finlisp_main([os.path.join(my_projects, "qs/financial", "chart-categories.lisp")],
                                   FILECONF('general', 'charts'),
                                   config,
                                   verbose,
                                   {'input-file': main_account,
                                    'statements-file': FILECONF('finance', 'accumulated-bank-statements-file'),
                                    'classifiers-file': CONF('finance', 'budgeting-classes-file'),
                                    'thresholds-file': CONF('finance', 'thresholds-file'),
                                    'verbose': verbose})

    if file_newer_than_file(main_account, FILECONF('finance', 'finances-completions')):
        if verbose: print("updating finances completions")
        list_completions.list_completions()

def update_physical(begin_date, end_date):

    """Merge incoming health-related data from various files, into one central file."""

    charts_dir = FILECONF('general', 'charts')
    physical = FILECONF('physical', 'physical-filename')
    mfp_filename = FILECONF('physical', 'mfp-filename')
    phys_scratch = "/tmp/physical-tmp.csv"
    archive_dir = FILECONF('backups', 'archive')

    physical_files = [FILECONF('physical', 'weight-filename')
                     # TODO: merge the other physical files
                    ]

    qsmerge.qsmerge(physical,
                    physical_files, None, phys_scratch)

    if qsutils.qsutils.check_merged_row_dates.check_merged_row_dates(phys_scratch, physical, *physical_files):
        backup(physical, archive_dir, "physical-to-%s.csv")
        shutil.copy(phys_scratch, physical)
    else:
        print("merge of physical data produced the wrong number of rows")

def update_startpage():

    """Update my personal start page, for which the master is a YAML file."""

    startpage = FILECONF('start-page', 'startpage')
    startpage_source = FILECONF('start-page', 'startpage-source')
    startpage_style = FILECONF('start-page', 'startpage-style')
    if ((not os.path.isfile(startpage))
        or os.path.getmtime(startpage_source) > os.path.getmtime(startpage)
        or os.path.getmtime(startpage_style) > os.path.getmtime(startpage)):
        os.system("%s --output %s --stylesheet %s %s"
                  % (FILECONF('start-page', 'start-page-generator'), startpage, startpage_style, startpage_source))

def update_contacts():

    """Preen my contacts file.  This checks for links between contacts, and does some analysis, which it returns
    as the result."""

    contacts_file = FILECONF('contacts', 'contacts-file')
    contacts_scratch = "/tmp/contacts_scratch.csv"
    contacts_analysis = link_contacts.link_contacts_main(contacts_file, True, False, contacts_scratch)
    with open(contacts_file) as confile:
        original_lines = len(confile.readlines())
    with open(contacts_scratch) as conscratch:
        scratch_lines = len(conscratch.readlines())
    if original_lines == scratch_lines:
        backup(contacts_file, FILECONF('backups', 'archive'), "contacts-%s.csv")
        shutil.copy(contacts_scratch, contacts_file)
    else:
        print("wrong number of people after linking contacts, originally", original_lines, "but now", scratch_lines)
    return contacts_analysis

def update_agenda():
    """Also updates the parcels expected list.
    Files written:
    * $SYNCED/var/views.json
    * $SYNCED/var/parcels-expected.json"""
    os.system("emacs -q --script " +
              os.path.expandvars("$MY_ELISP/special-setups/dashboard/dashboard-emacs-query.el"))

COVID_UK_URL = "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newAdmissions&metric=newCasesByPublishDate&metric=newCasesLFDOnlyBySpecimenDate&metric=newDeaths60DaysByPublishDate&metric=newVaccinesGivenByPublishDate&format=csv"

def update_covid():
    """Fetch the UK Covid-19 data."""
    covid = {row['date']: row
             for row in csv.DictReader(
                     io.StringIO(
                         requests.get(
                             COVID_UK_URL).text))}
    # TODO: write to file
    # TODO: include in charts

def rename_columns(raw, column_renames):
    return ({column_renames.get(key, key): value for key, value in raw.items()}
            if isinstance(raw, dict)
            else [column_renames.get(key, key) for key in raw])

def transform_cells(row, transformations):
    return {k: transformations.get(k, lambda a: a)(v)
            for k, v in row.items()}

def matches(row, match_key, match_value):
    return (match_key is None
            or row.get(match_key) == match_value)

def merge_incoming_csv(main_file_key, incoming_key,
                       begin_date, end_date,
                       match_key=None, match_value=None,
                       column_renames={},
                       transformations={}):

    """Merge a CSV of new (typically daily) readings into a long-term history, by date.  The incoming data may
    overlap with the data already in the file, in which case the new data takes precedence; it won't make
    duplicate rows.  Columns may be renamed between the two files, and the data may transformed, according
    to the argument dictionaries 'column_renames' and 'transformations'."""

    main_filename = FILECONF('physical', main_file_key)
    incoming_filename = latest_file_matching(FILECONF('physical', incoming_key))
    if incoming_filename:
        print("merging", incoming_filename, "into", main_filename, "with column_renames", column_renames, "and matches", match_key, match_value)
        qsutils.qsutils.trim_csv.trim_csv(incoming_filename)
        if (os.path.isfile(incoming_filename)
            and ((not os.path.isfile(main_filename))
                 or file_newer_than_file(incoming_filename, main_filename))):
            data = {}
            with open(incoming_filename) as instream:
                for row in csv.reader(instream):
                    header = rename_columns(row, column_renames)
                    break           # just get the first row
            if os.path.isfile(main_filename):
                with open(main_filename) as instream:
                    data = {row['Date']: row
                            for row in csv.DictReader(instream)}
            original_length = len(data)
            with open(incoming_filename) as instream:
                additional = {row['Date']: row
                              for row in (transform_cells(rename_columns(raw, column_renames),
                                                          transformations)
                                          for raw in csv.DictReader(instream))
                              if matches(row, match_key, match_value)}
                data.update(additional)
            if len(data) > original_length:
                backup(main_filename, FILECONF('backups', 'archive'), "%s-to-%%s.csv" % os.path.splitext(os.path.basename(main_filename))[0])
                with open(main_filename, 'w') as outstream:
                    writer = csv.DictWriter(outstream, header)
                    writer.writeheader()
                    for date in sorted(data.keys()):
                        writer.writerow(data[date])
        else:
            print("Latest incoming file", incoming_filename, "older than main file", main_filename)
    else:
        print("could not find an incoming file matching", FILECONF('physical', incoming_key), "for", incoming_key, "to merge into", main_filename)

def fetch_weather(_begin_date, _end_date, verbose):

    """Fetch the short-term forecast from openweathermap, saving hourly extracts from it into a CSV file, and
    the sunrise and sunset data into a JSON file."""

    owm = pyowm.owm.OWM(decouple.config('OWM_API_KEY'))
    reg = owm.city_id_registry()
    city = FILECONF('weather', 'weather-city')
    country = FILECONF('weather', 'weather-country')
    loc_name = "%s,%s" % (city, country)
    list_of_locations = reg.locations_for(city, country)
    place = list_of_locations[0]
    weather_manager = owm.weather_manager()
    observation = weather_manager.weather_at_place(loc_name)
    with open(FILECONF('weather', 'sunlight-times-file'), 'w') as outstream:
        json.dump({'sunrise': datetime.datetime.fromtimestamp(observation.weather.sunrise_time()).time().isoformat(timespec='minutes'),
                   'sunset': datetime.datetime.fromtimestamp(observation.weather.sunset_time()).time().isoformat(timespec='minutes')},
                  outstream)
    weather = weather_manager.one_call(lat=place.lat, lon=place.lon,units='metric')
    forecast = [{
        'time': datetime.datetime.fromtimestamp(h.ref_time).isoformat()[:16],
        'status': h.detailed_status,
        'precipitation': h.precipitation_probability,
        'temperature': h.temp['temp'],
        'uvi': h.uvi,
        'wind-speed': h.wnd['speed'],
        'wind-direction': h.wnd['deg']
    } for h in weather.forecast_hourly]

    with open(FILECONF('weather', 'weather-filename'), 'w') as outstream:
        writer = csv.DictWriter(outstream, ['time', 'status', 'precipitation', 'temperature', 'uvi', 'wind-speed', 'wind-direction'])
        writer.writeheader()
        for hour in forecast:
            writer.writerow(hour)

def fetch_mfp(_begin_date, _end_date, verbose):

    """Fetch recent data from MyFitnessPal.com."""

    if verbose: print("Fetching data from myfitnesspal.com (may take a little while)")
    physical.mfp_reader.update_mfp(FILECONF('physical', 'mfp-filename'), verbose)
    if verbose: print("Fetched data from myfitnesspal.com")

def fetch_oura(begin_date, end_date, verbose):

    """Update my Oura records by fetching updates from Oura's cloud system."""

    oura_filename = FILECONF('physical', 'oura-filename')
    data = {}
    physical.oura_reader.oura_read_existing(data, oura_filename)
    existing_rows = len(data)
    if begin_date is None:
        begin_date = qsutils.qsutils.earliest_unfetched(data)
    if verbose: print("fetching data from oura")
    physical.oura_reader.oura_fetch(data, begin_date, end_date)
    if verbose: print("fetched data from oura")
    if len(data) > existing_rows:
        backup(oura_filename, FILECONF('backups', 'archive'), "oura-to-%s.csv")
        physical.oura_reader.oura_write(data, oura_filename)
    elif len(data) < existing_rows:
        print("Warning: sleep data has shrunk on being fetched --- not writing it")

def fetch_omron(begin_date, end_date, verbose):

    """Extract data from a manually saved Omron file.

    Automatic fetcher or scraper possibly to come."""

    if verbose: print("Updating blood pressure data")

    merge_incoming_csv('omron-filename', 'omron-incoming-pattern',
                       begin_date, end_date,
                       column_renames={'Measurement Date': 'Date',
                                       'Time Zone': 'Timezone',
                                       'SYS(mmHg)': 'SYS',
                                       'DIA(mmHg)': 'DIA',
                                       'Pulse(bpm)': 'Pulse',
                                       'Device': 'Device Model Name'},
                       transformations={'Irregular heartbeat detected': qsutils.qsutils.string_to_bool,
                                        'Body Movement': qsutils.qsutils.string_to_bool,
                                        'Date': qsutils.qsutils.normalize_date})

def fetch_running(begin_date, end_date, verbose):

    """Extract running records from the latest saved Garmin data.  Garmin don't provide API access to
    individuals, so for now I'm saving the file manually from their web page --- automatic button pusher
    possibly to follow."""

    if verbose: print("updating running data")

    merge_incoming_csv('running-filename', 'garmin-incoming-pattern',
                       begin_date, end_date,
                       match_key='Activity Type', match_value='Running',
                       transformations={'Time': qsutils.qsutils.duration_string_to_minutes,
                                        'Calories': qsutils.qsutils.string_to_number})

def fetch_cycling(begin_date, end_date, verbose):

    """Extract cycling records from the latest saved Garmin data.  Garmin don't provide API access to
    individuals, so for now I'm saving the file manually from their web page --- automatic button pusher
    possibly to follow."""

    if verbose: print("updating cycling data")

    merge_incoming_csv('cycling-filename', 'garmin-incoming-pattern',
                       begin_date, end_date,
                       match_key='Activity Type', match_value='Cycling',
                       transformations={'Time': qsutils.qsutils.duration_string_to_minutes,
                                        'Calories': qsutils.qsutils.string_to_number})

def fetch_travel(begin_date, end_date, verbose):
    # TODO: fetch from Google, updating FILECONF('travel', 'travel-filename') and FILECONF('travel', 'places-filename')

    if verbose: print("fetching travel data")

    pass

def update_travel():
    # TODO: write travel section of QS code
    # travel_main(FILECONF('travel', 'travel-filename'), FILECONF('travel', 'places-filename'))
    # TODO: calculate distances
    pass

def updates(begin_date, end_date,
            do_externals,
            verbose=False,
            testing=False,
            force=False):

    """Update my Quantified Self record files, which are generally CSV files with a row for each day.  This also
    prepares some files for making charts.  Finally, it calls the dashboard code, which uses matplotlib to
    make the charts, as well as generating the HTML they are embedded in.

    The argument do_externals says whether to contact any external data sources."""

    global CONFIGURATION
    CONFIGURATION = lifehacking_config.load_config()

    os.makedirs(FILECONF('general', 'charts'), exist_ok=True)
    # if end_date is None:
    #     end_date = qsutils.qsutils.yesterday()

    if do_externals:
        if verbose: print("Fetching external data")
        for location_name, fetcher, archive_template in [
                (('weather', 'weather-filename'), fetch_weather, "weather-to-%s.csv"),
                (('physical', 'mfp-filename'), fetch_mfp, "mfp-to-%s.csv"),
                (('travel', 'travel-filename'), fetch_travel, "travel-to-%s.csv"),
                (('physical', 'oura-filename'), fetch_oura, "oura-to-%s.csv"),
                (('physical', 'omron-filename'), fetch_omron, "omron-to-%s.csv"),
                (('physical', 'cycling-filename'), fetch_cycling, "cycling-to-%s.csv"),
                (('physical', 'running-filename'), fetch_running, "running-to-%s.csv")
                # TODO: add elliptical trainer, planks
                ]:
            filename = FILECONF(*location_name)
            if force or last_update_at_least_about_a_day_ago(filename):
                if verbose: print("updating", filename)
                backup(filename, FILECONF('backups', 'archive'), archive_template)
                fetcher(begin_date, end_date, verbose)
            else:
                if verbose: print("not updating", filename, "as it is recent")

    update_finances(verbose)
    update_physical(begin_date, end_date)
    contacts_analysis = update_contacts()
    update_agenda()
    update_travel()
    update_startpage()
    dashboard.dashboard.make_dashboard_page(contacts_analysis=contacts_analysis,
                                            chart_sizes=CHART_SIZES)

def main():
    parser = qsutils.qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/private_html/dashboard"),
                        help="""Directory to write charts into.""")
    parser.add_argument("--begin",
                        help="""Earliest date to chart.""")
    parser.add_argument("--end",
                        help="""Latest date to chart.""")
    parser.add_argument("--no-externals", action='store_true',
                        help="""Don't pester external servers""")
    parser.add_argument("--force", action='store_true',
                        help="""Do the updates even if the files have been updated
                        within the last day.""")
    parser.add_argument("--testing", action='store_true',
                        help="""Use an alternate directory which can be reset.""")
    args = parser.parse_args()

    updates(args.begin,
            args.end,
            not args.no_externals,
            verbose=args.verbose,
            force=args.force,
            testing=args.testing)

if __name__ == '__main__':
    main()
