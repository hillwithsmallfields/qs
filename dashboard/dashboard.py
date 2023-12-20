#!/usr/bin/env python3

import cssutils
import csv
import datetime
import glob
import json
import os
import random
import shutil
import sys

import numpy as np

import dobishem.dates
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, SectionalPage
import expressionive.exprpages as exprpages

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

source_dir = os.path.dirname(os.path.realpath(__file__))

# other parts of this project group
ensure_in_path(os.path.dirname(source_dir))
import financial.classify       # https://github.com/hillwithsmallfields/qs/blob/master/financial/classify.py
import financial.spending_chart
import financial.parentage
import financial.finutils
import qsutils.qsutils            # https://github.com/hillwithsmallfields/qs/blob/master/utils/qsutils.py
import qsutils.qschart
import qsutils.html_pages

import channels.timetable
from expressionive.expridioms import switchable_panel, linked_image

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

import coimealta.contacts.contacts_data as contacts_data
import coimealta.inventory.storage as storage
import coimealta.inventory.perishables as perishables

ensure_in_path(os.path.join(my_projects, "noticeboard"))

import announce                 # https://github.com/hillwithsmallfields/noticeboard/blob/master/announce.py
import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

def make_remaining_cell(thresholds, spent_this_month, coi):
    """Return a table cell showing how much is left in a budget category."""
    # TODO: put this back into use
    available = float(thresholds.get(coi, 0))
    spent_string = spent_this_month.get(coi, "")
    spent = 0 if spent_string == "" else float(spent_string)
    return T.td(class_='ok' if spent <= available else "overspent")[str(available + spent)]

def dashboard_page_colours():
    """Return the foreground and background colours, and a shading colour, specified in the stylesheet."""

    sheet = cssutils.parseFile(os.path.join(source_dir, "dashboard.css"))

    foreground = None
    shading = None
    background = None

    for rule in sheet:
        if rule.selectorText == 'BODY':
            for property in rule.style:
                if property.name == 'color':
                    foreground = property.value
                    continue
                if property.name == 'background-color':
                    background = property.value
                    continue
        elif rule.selectorText == 'H1':
            for property in rule.style:
                if property.name == 'background':
                    shading = property.value
                    continue
        if foreground and background and shading:
            break

    return foreground, background, shading

def peak_flow_section():
    # TODO: get peak flow data
    return None

def calories_section():
    return linked_image("total_calories", "total_calories")

def meals_section():
    return linked_image("meal_calories", "meal_calories")

def calories_per_day_of_week():
    # TODO: make file with calories split by day of week, and display that
    return None

def foods_section():
    return linked_image("origin_calories", "origins")

def running_section():
    return linked_image("running", "running",
                        fallback=T.p["Fetch data from ",
                                     T.a(href=FILECONF('physical', 'garmin-manual-fetching-page'))["Garmin page"],
                                     " using the Export CSV button"])

def cycling_section():
    return linked_image("cycling", "cycling",
                        fallback=T.p["Fetch data from ",
                                     T.a(href=FILECONF('physical', 'garmin-manual-fetching-page'))["Garmin page"],
                                     " using the Export CSV button"])

def sleep_split_section():
    return linked_image("sleep-split", "sleep-split")

def sleep_times_section():
    return linked_image("sleep-times", "sleep-times")

def sleep_correlation_section():
    return None

def blood_pressure_section():
    return linked_image("blood-pressure", "blood-pressure",
                        fallback=T.p["Tap the 'Blood pressure' section of the app display, then use the menu to mail the file to yourself."])

def temperature_section():
    return linked_image("temperature", "temperature")

def travel_section():
    # TODO: read travel.csv and a journeys file generated from Google
    return None

def construct_dashboard_page(charts_dir, channels_data):
    page = SectionalPage()
    page.add_section(None,
                     wrap_box(T.div[T.h2["Perishable food to use up"],
                                    channels_data['perishables'].html(),
                                    T.h2["Parcels expected"],
                                    channels_data['parcels'].html()],
                              channels_data['timetable'].html(),
                              channels_data['weather'].html()),)
    page.add_section("Health", wrap_box(
        *[
            channels_data[key].html()
            for key in [
                    'weight',
            ]
        ]
        # labelled_section("Calories", calories_section()),
        # labelled_section("Meals", meals_section()),
        # labelled_section("By day of week", calories_per_day_of_week()),
        # labelled_section("Food groups", foods_section()),
        # labelled_section("Running", running_section()),
        # labelled_section("Cycling", cycling_section()),
        # labelled_section("Walking", walking_section()),
        # labelled_section("Blood pressure", blood_pressure_section()),
        # labelled_section("Peak flow", peak_flow_section()),
        # labelled_section("Sleep split", sleep_split_section()),
        # labelled_section("Sleep times", sleep_times_section()),
        # labelled_section("Sleep correlation", sleep_correlation_section()),
        # labelled_section("Temperature", temperature_section())
    ))
    for panel_key in [
            'agenda',
            'finances',
            'contacts',
            'travel',
            'inventory',
            'reflections',
    ]:
        handler = channels_data[panel_key]
        page.add_section(handler.label(), handler.html())
    # page.add_section("Travel", travel_section())
    # page.add_section("Inventory", inventory_section())
    # page.add_section("Texts for reflection", reflection_section())
    return [T.body(onload="init_dashboard()")[
        T.script(src="dashboard.js"),
        T.h1["Personal dashboard"],
        page.toc(),
        page.sections()]]

def write_dashboard_page(charts_dir,
                         channels_data,
                         details_background_color="gold", inline=True):
    """Construct and save the dashboard page."""
    with open(os.path.join(charts_dir, "index.html"), 'w') as page_stream:
        page_stream.write(
            exprpages.page_text(
                construct_dashboard_page(charts_dir, channels_data),
                ((exprpages.tagged_file_contents(
                    "style", os.path.join(source_dir, "dashboard.css"))
                 + qsutils.qsutils.table_support_css(details_background_color))
                 if inline
                 else ""),
                (exprpages.tagged_file_contents(
                    "script", os.path.join(source_dir, "dashboard.js"))
                 if inline
                 else "")))
    if not inline:
        for filename in ("dashboard.css",
                         "dashboard.js"):
            shutil.copy(os.path.join(source_dir, filename),
                        os.path.join(charts_dir, filename))

def make_dashboard_images(charts_dir,
                          channels_data,
                          chart_sizes,
                          background_colour,
                          begin_date=None, end_date=None,
                          verbose=False):
    """Make all the images for the dashboard."""
    now = datetime.datetime.now()
    today = now.date()
    for param_set in chart_sizes.values():
        param_set['facecolor'] = background_colour

    periods = {'all_time': datetime.date(year=1973, month=1, day=1),
               'past_week': dobishem.dates.back_from(today, None, None, 7),
               'past_month': dobishem.dates.back_from(today, None, 1, None),
               'past_quarter': dobishem.dates.back_from(today, None, 3, None),
               'past_year': dobishem.dates.back_from(today, 1, None, None)}
    for channel in channels_data.values():
        for date_suffix, begin in ({'custom': begin_date}
                                   if begin_date
                                   else periods).items():
            channel.prepare_page_images(
                date_suffix=date_suffix,
                begin_date=np.datetime64(datetime.datetime.combine(begin, now.time())),
                end_date=np.datetime64(now),
                chart_sizes=chart_sizes)

def make_dashboard_page(charts_dir=None,
                        channels_data=None,
                        chart_sizes={'small': {'figsize': (5,4)},
                                     'large': {'figsize': (11,8)}},
                        begin_date=None, end_date=None,
                        verbose=False):

    """Make the dashboard page, including refreshed images for it."""

    if not charts_dir:
        charts_dir = os.path.expanduser("~/private_html/dashboard")

    text_colour, background_colour, shading = dashboard_page_colours()
    make_dashboard_images(charts_dir,
                          channels_data,
                          chart_sizes,
                          background_colour,
                          begin_date, end_date,
                          verbose)
    write_dashboard_page(charts_dir,
                         channels_data,
                         details_background_color=shading)

def main():
    parser = qsutils.qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/private_html/dashboard"),
                        help="""Directory to write charts into.""")
    args = parser.parse_args()

    make_dashboard_page(charts_dir=args.charts)

if __name__ == '__main__':
    main()
