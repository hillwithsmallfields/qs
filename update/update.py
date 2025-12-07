#!/usr/bin/env python3

import sys
print("using python version", sys.version)
import argparse
import concurrent.futures
import csv
import datetime
import io
import inotify.adapters
import json
import os
import re
import requests
import shutil
import time
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
import channels.bible
import channels.contacts
import channels.finances
import channels.inventory
import channels.parcels
import channels.perishables
import channels.physical
import channels.reflections
import channels.startpage
import channels.timetable
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

def update_once(public_handlers,
                private_handlers,
                store, charts, private_charts,
                begin, end,
                no_externals,
                serial=False,
                verbose=False,
                testing=False,
                force=False):

    with BeginAndEndMessages("archiving old data", verbose=verbose) as msgs:
        msgs.print("in update_once, charts has %d templates: %s" % (len(charts.templates), charts))
        files_subject_to_change = [filename
                                   for channel in (public_handlers + private_handlers)
                                   for filename in channel.files_to_write()
                                   if os.path.exists(filename)]
        if files_subject_to_change:
            backup.backup(files_subject_to_change,
                          os.path.expanduser("~/archive"),
                          messager=msgs)

    with BeginAndEndMessages("updating data and refreshing dashboard", verbose=verbose):
        all_handlers = (public_handlers + private_handlers)
        if not no_externals:
            if serial:
                for handler in all_handlers:
                    handler.fetch(verbose=verbose, messager=msgs)
            else:
                with concurrent.futures.ThreadPoolExecutor(max_workers=len(all_handlers)) as ex:
                    with BeginAndEndMessages("fetching external data", verbose=verbose) as msgs:
                        ex.map(lambda handler: handler.fetch(verbose=verbose, messager=msgs),
                               all_handlers)

        with BeginAndEndMessages("updating saved data", verbose=verbose) as msgs:
            if serial:
                for handler in all_handlers:
                    handler.update(verbose=verbose, messager=msgs)
            else:
                with concurrent.futures.ThreadPoolExecutor(max_workers=len(all_handlers)) as ex:
                    ex.map(lambda handler: handler.update(verbose=verbose, messager=msgs),
                           all_handlers)

        with BeginAndEndMessages("refreshing dashboard", verbose=verbose) as msgs:
            msgs.print("about to make dashboard page; charts has %d templates: %s" % (len(charts.templates), charts))
            dashboard.dashboard.make_dashboard_pages(
                store=store, charts=charts, private_charts=private_charts,
                public_channels_data={
                    handler.name(): handler
                    for handler in public_handlers
                },
                private_channels_data={
                    handler.name(): handler
                    for handler in private_handlers
                },
                chart_sizes=CHART_SIZES,
                verbose=verbose)

STORAGE_TEMPLATES = {
    'scratch': "var/%(scratch)s",
    'texts': "texts/%(texts)s",
    'organizational': "org/%(organizational)s",
    'health': "health/%(health)s",
    'finances': "finances/%(finances)s",
    'finances_subdir': "finances/%(subdir)s/%(finances)s",
    'ringing': "ringing/%(ringing)s",
    'timetables': "timetables/%(timetables)s",
    'downloads': "~/Downloads/%(downloads)s",
}

CHART_TEMPLATES = {
    'page': "%(page)s.html",
    'chart': "%(chart)s.png",
    'financial': "%(financial)s.csv",
    'sized_chart': "%(chart)s-%(size)s.png",
    'dated_sized_chart': "%(date_suffix)s-%(size)s.png",
    'type_dated_sized_chart': "%(chart_type)s-%(date_suffix)s-%(size)s.png",
    'activity_chart': "%(activity)s-%(size)s.png",
    'period_activity_chart': "%(activity)s-%(date_suffix)s-%(size)s.png",
    'period_name_chart': "%(date_suffix)s-%(name_suffix)s.png",
    'weight_chart': "weight-%(weight_units)s-%(date_suffix)s-%(size)s.png",
    'misc_chart': "%(chart_type)s-%(name_suffix)s-%(date_suffix)s.png",
}

def updates(charts,
            begin, end,
            no_externals,
            serial=False,
            verbose=False,
            testing=False,
            force=False,
            watch=False,
            delay=15):

    """Update my Quantified Self record files, which are generally CSV
    files with a row for each day.  This also prepares some files for
    making charts.  Finally, it calls the dashboard code, which uses
    matplotlib to make the charts, as well as generating the HTML they
    are embedded in.

    The argument read_externals says whether to contact any external data sources.

    """

    if end is None:
        end = dates.yesterday()

    print("setting up storage with templates", STORAGE_TEMPLATES)
    store = storage.Storage(
        templates=STORAGE_TEMPLATES,
        defaults={},
        base="$SYNCED")
    print(len(CHART_TEMPLATES), "chart templates are:", CHART_TEMPLATES)
    outputs = storage.Storage(
        templates=CHART_TEMPLATES,
        defaults={},
        base="~/private_html/dashboard")
    print(len(outputs.templates), "output templates are:", outputs.templates)
    private_outputs = storage.Storage(
        templates=CHART_TEMPLATES,
        defaults={},
        base="~/private_html/dashboard/me")
    print(len(private_outputs.templates), "private output templates are:", private_outputs.templates)
    public_handlers = [
        panel_class(store, outputs)
        for panel_class in [
                channels.weather.WeatherPanel,
                channels.reflections.ReflectionsPanel,
                channels.bible.BiblePanel,
                channels.startpage.StartPage,
        ]
    ]
    private_handlers = [
        panel_class(store, outputs)
        for panel_class in [
                channels.finances.FinancesPanel,
                # channels.weight.WeightPanel,
                channels.parcels.ParcelsPanel,
                channels.timetable.TimetablePanel,
                channels.agenda.AgendaPanel,
                channels.physical.PhysicalPanel,
                channels.contacts.ContactsPanel,
                channels.perishables.PerishablesPanel,
                channels.travel.TravelPanel,
                channels.inventory.InventoryPanel,
                channels.ringing.RingingPanel,
        ]
    ]

    if watch:
        i = inotify.adapters.Inotify()
        i.add_watch(os.path.expandvars("$ORG"))
        i.add_watch(os.path.expandvars("$SYNCED/health"))
        i.add_watch(os.path.expandvars("$SYNCED/travel"))

        for event in i.event_gen(yield_nones=False):
            (_, type_names, path, filename) = event
            if 'IN_CLOSE_WRITE' in type_names:
                print("PATH=[{}] FILENAME=[{}] EVENT_TYPES={}".format(path, filename, type_names))
                # allow for multiple files to be saved in a burst
                time.sleep(delay)
                update_once(public_handlers,
                            private_handlers,
                            store, outputs, private_outputs,
                            begin, end,
                            no_externals,
                            serial,
                            verbose,
                            testing,
                            force)
    else:
        update_once(public_handlers,
                    private_handlers,
                    store, outputs, private_outputs,
                    begin, end,
                    no_externals,
                    serial,
                    verbose,
                    testing,
                    force)

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
    parser.add_argument("--watch", action='store_true',
                        help="""Watch the input directories, in an inotify loop.""")
    parser.add_argument("--delay", type=int,
                        help="""Delay in seconds between detecting a file change and running an update.
                        Allows for saving of multiple files in a burst.""")
    parser.add_argument("--verbose", action='store_true',
                        help="""Output more progress messages.""")
    return vars(parser.parse_args())

if __name__ == '__main__':
    updates(**get_args())
