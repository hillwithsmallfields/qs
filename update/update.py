#!/usr/bin/env python3

import argparse
import csv
import datetime
import decouple
import io
import json
import os
import re
import requests
import shutil
import sys
import yaml

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

# other parts of this project group:
ensure_in_path(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import factotum
import dashboard.dashboard
import qsutils.check_merged_row_dates
import financial.list_completions
import physical.mfp_reader
import physical.oura_reader
import qsutils.trim_csv
import qsutils.qsmerge
import qsutils
import channels.agenda
import channels.contacts
import channels.finances
import channels.physical
import channels.startpage
import channels.travel
import channels.weather

import backup

my_projects = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
ensure_in_path(os.path.join(my_projects, "coimealta/contacts"))

ensure_in_path(os.path.join(my_projects, "noticeboard"))

import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

CHART_SIZES = {'small': {'figsize': (5,4)},
               'large': {'figsize': (11,8)}}

def file_newer_than_file(a, b):
    return os.path.getmtime(a) > os.path.getmtime(b)

def last_update_at_least_about_a_day_ago(filename):
    return ((not os.path.isfile(filename))
            or ((datetime.datetime.fromtimestamp(os.path.getmtime(filename))
                 + datetime.timedelta(hours=23, minutes=30))
                < datetime.datetime.now()))

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
    """Returns a row dictionary or a header list with columns renamed."""
    return ({column_renames.get(key, key): value for key, value in raw.items()}
            if isinstance(raw, dict)
            else [column_renames.get(key, key) for key in raw])

def transform_cells(row, transformations):
    """Returns a row dict with column-specific transformations applied."""
    return {k: transformations.get(k, lambda a: a)(v)
            for k, v in row.items()}

def matches(row, match_key, match_value):
    """Returns whether a row contains a given value in a given column.
    If no column is given, returns True."""
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

    main_filename = facto.file_config('physical', main_file_key)
    incoming_filename = latest_file_matching(facto.file_config('physical', incoming_key))
    if incoming_filename:
        print("merging", incoming_filename, "into", main_filename, "with column_renames", column_renames, "and matches", match_key, match_value)
        qsutils.qsutils.trim_csv.trim_csv(incoming_filename) # Remove leading guff bytes added by financisto
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
                backup.backup(main_filename, facto.file_config('backups', 'archive'), "%s-to-%%s.csv" % os.path.splitext(os.path.basename(main_filename))[0])
                with open(main_filename, 'w') as outstream:
                    writer = csv.DictWriter(outstream, header)
                    writer.writeheader()
                    for date in sorted(data.keys()):
                        writer.writerow(data[date])
            return data
        else:
            print("Latest incoming file", incoming_filename, "older than main file", main_filename)
            return None
    else:
        print("could not find an incoming file matching", facto.file_config('physical', incoming_key), "for", incoming_key, "to merge into", main_filename)
        return None

class BeginAndEndMessages:

    def __init__(self, about, verbose):
        self.verbose = verbose
        self.about = about

    def __enter__(self):
        if self.verbose:
            print("Beginning " + self.about)

    def __enter__(self, exc_type, _exc_val, _exc_tb):
        if self.verbose:
            print(("Abandoned" if exc_type else "Finished ") + self.about)

def fetch_mfp(facto, _begin_date, _end_date, verbose):

    """Fetch recent data from MyFitnessPal.com."""

    with BeginAndEndMessages("fetching data from myfitnesspal.com", verbose):
        return physical.mfp_reader.MFP(facto.file_config('physical', 'mfp-filename')).update(verbose)

def fetch_oura(facto, begin_date, end_date, verbose):

    """Update my Oura records by fetching updates from Oura's cloud system."""

    oura_filename = facto.file_config('physical', 'oura-filename')
    data = {}
    physical.oura_reader.oura_read_existing(data, oura_filename)
    existing_rows = len(data)
    if begin_date is None:
        begin_date = qsutils.qsutils.earliest_unfetched(data)
    with BeginAndEndMessages("fetching data from oura", verbose):
        physical.oura_reader.oura_fetch(data, begin_date, end_date)
    if len(data) > existing_rows:
        backup.backup(oura_filename, facto.file_config('backups', 'archive'), "oura-to-%s.csv")
        physical.oura_reader.oura_write(data, oura_filename)
    elif len(data) < existing_rows:
        print("Warning: sleep data has shrunk on being fetched --- not writing it")
    return data

def fetch_omron(facto, begin_date, end_date, verbose):

    """Extract data from a manually saved Omron file.

    Automatic fetcher or scraper possibly to come."""

    with BeginAndEndMessages("updating blood pressure data"):
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

def fetch_running(facto, begin_date, end_date, verbose):

    """Extract running records from the latest saved Garmin data.  Garmin don't provide API access to
    individuals, so for now I'm saving the file manually from their web page --- automatic button pusher
    possibly to follow."""

    # There may be some movement on this: https://support.garmin.com/en-US/?faq=W1TvTPW8JZ6LfJSfK512Q8

    with BeginAndEndMessages("updating running data", verbose):
        return merge_incoming_csv('running-filename', 'garmin-incoming-pattern',
                                  begin_date, end_date,
                                  match_key='Activity Type', match_value='Running',
                                  transformations={'Time': qsutils.qsutils.duration_string_to_minutes,
                                                   'Calories': qsutils.qsutils.string_to_number})

def fetch_cycling(facto, begin_date, end_date, verbose):

    """Extract cycling records from the latest saved Garmin data.  Garmin don't provide API access to
    individuals, so for now I'm saving the file manually from their web page --- automatic button pusher
    possibly to follow."""

    with BeginAndEndMessages("updating cycling data", verbose):
        merge_incoming_csv('cycling-filename', 'garmin-incoming-pattern',
                           begin_date, end_date,
                           match_key='Activity Type', match_value='Cycling',
                           transformations={'Time': qsutils.qsutils.duration_string_to_minutes,
                                            'Calories': qsutils.qsutils.string_to_number})

def fetch_data(facto, filename, begin_date, end_date, verbose):
    filename = facto.file_config(*location_name)
    if force or last_update_at_least_about_a_day_ago(filename):
        with BeginAndEndMessages("updating " + filename, verbose):
            backup.backup(filename, facto.file_config('backups', 'archive'), archive_template)
            return fetcher(facto, begin_date, end_date, verbose)
    else:
        if verbose: print("not updating", filename, "as it is recent")
        return None             # TODO: read the data instead

def updates(charts_dir,
            begin_date, end_date,
            read_externals,
            verbose=False,
            testing=False,
            force=False):

    """Update my Quantified Self record files, which are generally CSV
    files with a row for each day.  This also prepares some files for
    making charts.  Finally, it calls the dashboard code, which uses
    matplotlib to make the charts, as well as generating the HTML they
    are embedded in.

    The argument read_externals says whether to contact any external data sources.

    """

    facto = factotum.Factotum(lifehacking_config.load_config())

    os.makedirs(facto.file_config('general', 'charts'), exist_ok=True)
    # if end_date is None:
    #     end_date = qsutils.qsutils.yesterday()

    if read_externals:
        with BeginAndEndMessages("fetching external data", verbose):
            weather = channels.weather.Weather(
                facto, begin_date, end_date, verbose
            ).fetch()
            combined_data = [
                fetch_data(facto, filename, begin_date, end_date, verbose)
                for location_name, fetcher, archive_template in [
                        # (('weather', 'weather-filename'), fetch_weather, "weather-to-%s.csv"),
                        (('physical', 'mfp-filename'), fetch_mfp, "mfp-to-%s.csv"),
                        (('travel', 'travel-filename'), fetch_travel, "travel-to-%s.csv"),
                        # (('physical', 'oura-filename'), fetch_oura, "oura-to-%s.csv"),
                        (('physical', 'omron-filename'), fetch_omron, "omron-to-%s.csv"),
                        (('physical', 'cycling-filename'), fetch_cycling, "cycling-to-%s.csv"),
                        (('physical', 'running-filename'), fetch_running, "running-to-%s.csv")
                        # TODO: add elliptical trainer, planks
                    ]]

    dashboard.dashboard.make_dashboard_page(
        facto,
        charts_dir=charts_dir,
        channel_data={
            name: handler(facto).update(read_externals, verbose)
            for name, handler in {
                    'finances': channels.finances.Finances,
                    'physical': channels.physical.Physical,
                    'contacts': channels.contacts.Contacts,
                    'agenda': channels.agenda.Agenda,
                    'timetable': channels.timetable.Timetable,
                    'travel': channels.travel.Travel,
                    'startpage': channels.startpage.StartPage,
                    'weather': channels.weather.Weather,
            }.items()},
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
