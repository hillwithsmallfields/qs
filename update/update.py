#!/usr/bin/env python3

import argparse
import concurrent.futures
import csv
import datetime
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

# my utils
import dobishem.data as data
import dobishem.dates as dates
import dobishem.storage as storage
from dobishem.nested_messages import BeginAndEndMessages

# other parts of this project group:
ensure_in_path(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import dashboard.dashboard
import qsutils.trim_csv
import qsutils
import channels.agenda
import channels.contacts
import channels.finances
import channels.inventory
import channels.parcels
import channels.perishables
import channels.physical
import channels.reflections
import channels.startpage
import channels.travel
import channels.ringing
import channels.weather

import backup

my_projects = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
ensure_in_path(os.path.join(my_projects, "coimealta/contacts"))

ensure_in_path(os.path.join(my_projects, "noticeboard"))

CHART_SIZES = {'small': {'figsize': (5,4)},
               'large': {'figsize': (11,8)}}

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

def updates(charts,
            begin, end,
            no_externals,
            serial=False,
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

    os.makedirs(os.path.expanduser("~/private_html/dashboard"), exist_ok=True)

    if end is None:
        end = dates.yesterday()

    handlers = [
        panel_class(charts)
        for panel_class in [
                channels.finances.FinancesPanel,
                # channels.weight.WeightPanel,
                channels.parcels.ParcelsPanel,
                channels.timetable.TimetablePanel,
                channels.weather.WeatherPanel,
                channels.agenda.AgendaPanel,
                channels.physical.PhysicalPanel,
                channels.contacts.ContactsPanel,
                channels.reflections.ReflectionsPanel,
                channels.perishables.PerishablesPanel,
                channels.travel.TravelPanel,
                channels.inventory.InventoryPanel,
                channels.ringing.RingingPanel,
                channels.startpage.StartPage,
        ]
    ]

    with BeginAndEndMessages("archiving old data", verbose=verbose) as msgs:
        files_subject_to_change = [filename
                                   for channel in handlers
                                   for filename in channel.files_to_write()
                                   if os.path.exists(filename)]
        if files_subject_to_change:
            backup.backup(files_subject_to_change,
                          os.path.expanduser("~/archive"),
                          messager=msgs)

    with BeginAndEndMessages("updating data and refreshing dashboard", verbose=verbose):
        if not no_externals:
            if serial:
                for handler in handlers:
                    handler.fetch(verbose=verbose, messager=msgs)
            else:
                with concurrent.futures.ThreadPoolExecutor(max_workers=len(handlers)) as ex:
                    with BeginAndEndMessages("fetching external data", verbose=verbose) as msgs:
                        ex.map(lambda handler: handler.fetch(verbose=verbose, messager=msgs),
                               handlers)

        with BeginAndEndMessages("updating saved data", verbose=verbose) as msgs:
            if serial:
                for handler in handlers:
                    handler.update(verbose=verbose, messager=msgs)
            else:
                with concurrent.futures.ThreadPoolExecutor(max_workers=len(handlers)) as ex:
                    ex.map(lambda handler: handler.update(verbose=verbose, messager=msgs),
                           handlers)

        with BeginAndEndMessages("refreshing dashboard", verbose=verbose):
            dashboard.dashboard.make_dashboard_page(
                charts_dir=os.path.expanduser("~/private_html/dashboard"),
                channels_data={
                    handler.name(): handler
                    for handler in handlers
                },
                chart_sizes=CHART_SIZES,
                verbose=verbose)

def get_args():
    parser = argparse.ArgumentParser()
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
    parser.add_argument("--serial", action='store_true',
                        help="""Handle the channels serially, for easier debugging.""")
    parser.add_argument("--verbose", action='store_true',
                        help="""Output more progress messages.""")
    return vars(parser.parse_args())

if __name__ == '__main__':
    updates(**get_args())
