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

def namify(x):
    return x.replace(' ', '_')

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

def keep_in_touch_section():
    """List people who I mean to keep in touch with but haven't for a while."""
    people_by_id, _ = contacts_data.read_contacts("$SYNCED/org/contacts.csv")
    today = datetime.date.today()
    this_year = today.year
    long_uncontacted = [person
                        for person in people_by_id.values()
                        if contacts_data.contact_soon(person, today, days_since_last_contact=90)]
    if len(long_uncontacted) == 0:
        return T.p["No pending contacts."]
    return

def perishables_section():
    """List things to use up from the fridge, in order of expiry date."""
    items = perishables.get_perishables()
    now = datetime.datetime.now()
    today = now.date()
    week_ahead = (now + datetime.timedelta(days=7)).date()
    return (T.p["No items on record."]
            if len(items) == 0
            else T.table[
                    [T.tr[T.th(colspan="2")["Use by"],
                          T.th["Days left"],
                          T.th["Item"],
                          T.th["Quantity"]]],
                    [[T.tr(class_=("out_of_date"
                                   if row['Best before'] < today
                                   else ("use_soon"
                                         if row['Best before'] < week_ahead
                                         # TODO: convert near days to names
                                         else "use_later")))[
                                                 T.td[row['Best before'].isoformat()],
                                                 T.td[row['Best before'].strftime("%d")],
                                                 T.td(class_="days_left")[(row['Best before'] - today).days],
                                                 T.td[row['Product']],
                                                 T.td[str(row['Quantity'])]]
                      for row in items]]])

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

def top_items(items):
    return sorted(items, key=lambda item: item['position-in-file'])[:12]

def actions_section(channels_data):
    from_org_mode = channels_data['organizer']
    return wrap_box([T.h3["Mending"],
                     org_ql_list(top_items(from_org_mode["Mending"]))],
                    [T.h3["Physical making"],
                     org_ql_list(top_items(from_org_mode["Physical making"]))],
                    [T.h3["Programming"],
                     org_ql_list(top_items(from_org_mode["Programming"]))])

def org_ql_list(items):
    # TODO: make this scrollable
    return T.div(class_='agenda_list')[T.ul[[T.li[item['title']] for item in items]]]

def shopping_section(channels_data):
    from_org_mode = channels_data['organizer']
    return wrap_box([T.h3["Supermarket"],
                     org_ql_list(from_org_mode["Supermarket"])],
                    [T.h3["Online"],
                     org_ql_list(from_org_mode["Online"])])

def parcels_section(channels_data):
    parcels = channels_data['parcels']

def items_table(items):
    items_by_type = {}
    for item in items.values():
        key = "%s (%s)" % (item['Type'], item.get('Subtype', '?'))
        if key in items_by_type:
            items_by_type[key].append(item)
        else:
            items_by_type[key] = [item]
    return T.div(class_='inventory_list')[
        T.table[[T.tr[T.td[T.span(class_='overview')[key,
                                                     T.div(class_='details')[T.ul[[[T.li[x['Item']]]
                                                                                   for x in items_by_type[key]]]]]],
                      T.td[len(items_by_type[key])]]
                 for key in sorted(items_by_type.keys(),
                                   reverse=True,
                                   key=lambda k: len(items_by_type[k]))]]]

def inventory_section():

    locations = storage.read_locations(FILECONF('inventory', 'storage-file'))
    items = storage.read_inventory(FILECONF('inventory', 'inventory-file'))
    stock = storage.read_inventory(FILECONF('inventory', 'stock-file'))
    project_parts = storage.read_inventory(FILECONF('inventory', 'project-parts-file'))
    media = storage.read_books(FILECONF('inventory', 'books-file'))

    media_by_type = {}
    for medium in media.values():
        mediatype = medium['MediaType']
        if mediatype in media_by_type:
            media_by_type[mediatype].append(medium)
        else:
            media_by_type[mediatype] = [medium]

    books_with_acquisition_date = sorted([book
                                          for book in media_by_type['Book']
                                          if book.get('Acquired', None) is not None],
                                         key=lambda b: b['Acquired'])
    latest_book = books_with_acquisition_date[-1]

    _, volume, bookshelf_length, other_length, area = storage.calculate_capacities(locations)

    return T.div(class_='inventory')[
        T.div[wrap_box(T.div[T.h3["Media"],
                             T.div(class_='inventory_list')[
                                 T.dl[[T.div[T.dt[mtype],
                                             T.dd[str(len(media_by_type[mtype]))]] for mtype in sorted(media_by_type)]]]],
                       T.div[T.h3["General possessions"], items_table(items)],
                       T.div[T.h3["Project parts"], items_table(project_parts)],
                       T.div[T.h3["Stock"], items_table(stock)],
                       T.div[T.h3["Storage"],
                             T.div(class_='inventory_list')[T.dl[T.dt["Container volume"], T.dd["%g litres" % volume],
                                                                 T.dt["Bookshelf length"], T.dd["%g metres" % bookshelf_length],
                                                                 T.dt["Other shelf length"], T.dd["%g metres" % other_length]]]])]]

def travel_section():
    # TODO: read travel.csv and a journeys file generated from Google
    return None

def random_reflection(reflections_dir):
    with open(random.choice(glob.glob(os.path.join(reflections_dir, "*.txt")))) as instream:
        return random.choice([line.strip() for line in instream if line != "\n"])

def reflection_section():
    reflections_dir = FILECONF('general', 'reflections-dir')
    first = random_reflection(reflections_dir)
    second = random_reflection(reflections_dir)
    countdown = 4               # in case there's only one reflection available
    while second == first and countdown > 0:
        second = random_reflection(reflections_dir)
        countdown -= 1
    return T.div(class_='reflection')[
        T.p[first],
        T.p[second]]

def construct_dashboard_page(charts_dir, channels_data):
    page = SectionalPage()
    # TODO: move into panels structure
    # TODO: move into panels structure

    page.add_section(None,
                     # "Immediate",
                     wrap_box(T.div[T.h2["Perishable food to use up"],
                                    perishables_section(),
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
        # labelled_section("Blood pressure", blood_pressure_section()),
        # labelled_section("Peak flow", peak_flow_section()),
        # labelled_section("Sleep split", sleep_split_section()),
        # labelled_section("Sleep times", sleep_times_section()),
        # labelled_section("Sleep correlation", sleep_correlation_section()),
        # labelled_section("Temperature", temperature_section())
    ))
    for panel_key in [
            'finances',
            'contacts',
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

# def update_physical_charts(charts_dir,
#                            channels_data,
#                            chart_sizes,
#                            begin_date, end_date,
#                            date_suffix,
#                            vlines=None):

#     """Update the physical (health) charts."""

#     physical = "$SYNCED/health/physical.csv"
#     mfp_filename = "$SYNCED/health/mfp-accum.csv"

#     split_by_DoW = False

#     # TODO: rolling averages
#     for units in ('stone', 'kilogram', 'pound'):
#         qsutils.qschart.qscharts(physical,
#                                  'weight',
#                                  [units],
#                                  begin_date, end_date, None, split_by_DoW,
#                                  os.path.join(charts_dir, "weight-%s-%s-%%s.png" % (units, date_suffix)),
#                                  chart_sizes,
#                                  vlines=vlines)

#     for chartdef, template in [
#             ({'mainfile': mfp_filename,
#               'file_type': 'calories',
#               'columns': ['calories']},
#              "total_calories-%s-%%s.png"),
#             ({'mainfile': mfp_filename,
#               'file_type': 'meals',
#               'columns': ['breakfast', 'lunch', 'dinner', 'snacks']},
#              "meal_calories-%s-%%s.png"),
#             ({'mainfile': mfp_filename,
#               'file_type': 'food_groups',
#               'columns': ['carbohydrates', 'fat', 'protein', 'sugar']
#               }, "origin_calories-%s-%%s.png"),
#             # ({'mainfile': FILECONF('physical', 'oura-filename'),
#             #   'file_type': 'sleep',
#             #   'columns': ['Latency', 'Rem', 'Deep', 'Total']
#             #   }, "sleep-split-%s-%%s.png"),
#             ({'mainfile': "$SYNCED/health/blood-pressure.csv",
#               'file_type': 'blood_pressure',
#               'columns': ['systolic', 'diastolic', 'heart_rate']
#               }, "blood-pressure-%s-%%s.png"),
#             ({'mainfile': "$SYNCED/health/garmin-cycling.csv",
#               'file_type': 'cycling',
#               'columns': ['Distance', 'Calories', 'Time']
#               }, "cycling-%s-%%s.png"),
#             ({'mainfile': "$SYNCED/health/garmin-running.csv",
#               'file_type': 'running',
#               'columns': ['Distance', 'Calories', 'Time']
#               }, "running-%s-%%s.png"),
#             # ({'mainfile':,
#             #   'file_type':,
#             #   'columns':
#             #   }, ),
#     ]:
#         qsutils.qschart.qscharts(begin=begin_date, end=end_date,
#                                  match=None,
#                                  by_day_of_week=split_by_DoW,
#                                  plot_param_sets=chart_sizes,
#                                  outfile_template=os.path.join(charts_dir,
#                                                                template % date_suffix),
#                                  bar=False,
#                                  **chartdef)

#     sleep_chart_params = {suffix: chart.copy() for suffix, chart in chart_sizes.items()}
#     for scp in sleep_chart_params.values():
#         scp['subplot_kw'] = {'ylim': (0, 24.0)}
    # qsutils.qschart.qscharts(FILECONF('physical',
    #                                   'oura-filename'
    #                                   ), 'sleep',
    #                        ['Start', 'End'],
    #                        begin_date, end_date, None, split_by_DoW,
    #                        os.path.join(charts_dir, "sleep-times-%s-%%s.png" % date_suffix),
    #                        sleep_chart_params
    #                        # TODO: plt.ylim(0, 24) in the charting code
    # )
    # qsutils.qschart.qscharts(smart_one_filename, 'peak_flow',
    #                        ['Peak flow'],
    #                        begin_date, end_date, None,
    #                        os.path.join(charts_dir, "peak-flow-%s-%%s.png" % date_suffix),
    #                        chart_sizes)
    # qsutils.qschart.qscharts(FILECONF('physical', 'temperature-file'), 'temperature',
    #                        ['Temperature'],
    #                        begin_date, end_date, None,
    #                        os.path.join(charts_dir, "temperature-%s-%%s.png" % date_suffix),
    #                        chart_sizes)

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
