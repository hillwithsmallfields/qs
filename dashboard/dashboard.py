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

source_dir = os.path.dirname(os.path.realpath(__file__))

# other parts of this project group
sys.path.append(os.path.dirname(source_dir))
import financial.classify       # https://github.com/hillwithsmallfields/qs/blob/master/financial/classify.py
import qsutils.qsutils            # https://github.com/hillwithsmallfields/qs/blob/master/utils/qsutils.py
import qsutils.qschart

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

sys.path.append(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

sys.path.append(os.path.join(my_projects, "coimealta/contacts"))
import contacts_data            # https://github.com/hillwithsmallfields/coimealta/blob/master/contacts/contacts_data.py

sys.path.append(os.path.join(my_projects, "coimealta/inventory"))
import storage

sys.path.append(os.path.join(my_projects, "noticeboard"))

import announce                 # https://github.com/hillwithsmallfields/noticeboard/blob/master/announce.py
import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

sys.path.append(os.path.join(my_projects, "coimealta/inventory"))
import perishables              # https://github.com/hillwithsmallfields/coimealta/blob/master/inventory/perishables.py

CONFIGURATION = {}

def CONF(*keys):
    return lifehacking_config.lookup(CONFIGURATION, *keys)

def FILECONF(*keys):
    return os.path.expanduser(os.path.expandvars(CONF(*keys)))

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

def make_name_with_email(name, email):
    return (T.a(href="email:"+email)[name]
            if email and email != ""
            else name)

def row(*things):
    return T.table(width="100%")[T.tr[[T.td(valign="top")[thing] for thing in things]]]

def wrap_box(*things):
    return (T.div(class_='flex-container')[[T.div[thing]
                                            for thing in things
                                            if thing]]
            if any(things)
            else None)

def labelled_section(title, body):
    return T.div[T.h2[title], body] if body else None

class SectionalPage(object):

    """Holder for collecting section to make up a page.
    Each section has an H2 heading, and these are used to make a table of contents.
    Empty sections are not added."""

    pass

    def __init__(self):
        self._sections = []

    def add_section(self, title, body):
        if body:
            self._sections.append((title, body))

    def toc(self):
        return [T.h2["Table of contents"],
                T.ul[[T.li[T.a(href="#"+namify(section[0]))[section[0]]] for section in self._sections]]]

    def sections(self):
        return [[T.div(class_='section')[T.h2[T.a(name=namify(section[0]))[section[0]]],
                       T.div(class_='sectionbody')[section[1]]] for section in self._sections]]

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

def switchable_panel(switcher_id, panels, labels, order, initial):
    """Return a group of panels, only one of which is displayed at a time.
    panels is a dictionary binding keys to the panel contents,
    and labels is a dictionary binding the same keys to button labels.
    order is the order of the keys in the button row,
    and initial is the button to be selected initially."""
    return T.table(class_='switcher', id_=switcher_id)[
        T.tr(align="center")[T.td[[T.div(class_='choice', name=choice)[panels[choice]]
                                   for choice in order]]],
        T.tr(align="center")[
            T.td[[[T.button(class_=('active'
                                    if choice == initial
                                    else 'inactive'),
                            name=choice, onclick="select_version('%s', '%s')"%(switcher_id, choice))[labels[choice]]]
                  for choice in order]]]]

def linked_image(image_name, label, fallback=None):
    charts_dir = FILECONF('general', 'charts')
    os.path.join(charts_dir)
    periods = ('all_time', 'past_year', 'past_quarter', 'past_month', 'past_week')
    return switchable_panel(label,
                            {period: [
                                T.div(class_='choice', name=period)[
                                    (T.a(href="%s-%s-large.png" % (image_name, period))[
                                        T.img(src="%s-%s-small.png" % (image_name, period))]
                                     if os.path.isfile(os.path.join(charts_dir, "%s-%s-small.png" % (image_name, period))) # TODO: this isn't right, is it looking in the right directory?
                                     else fallback or T.p["Data needs fetching"])]
                                ]
                             for period in periods},
                            {period: period.capitalize().replace('_', ' ') for period in periods},
                            periods,
                            'past_quarter')

def recent_transactions_table(filename, days_back):
    start_date = qsutils.qsutils.back_from(datetime.date.today(), None, None, days_back)
    with open(filename) as instream:
        recent_transactions = [transaction
                           for transaction in csv.DictReader(instream)
                           if datetime.date.fromisoformat(transaction['date']) >= start_date]
    return T.div(class_='transactions_list')[
        T.table(class_='financial')[
            T.tr[T.th["Date"],T.th["Amount"],T.th["Payee"],T.th["Category"],T.th["Item"]],
            [[T.tr[T.th[transaction['date']],
                   T.td[transaction['amount']],
                   T.td[transaction['payee']],
                   T.td[transaction['category']],
                   T.td[transaction['item']]]
                  for transaction in reversed(recent_transactions)]]]]

def weight_section():
    return linked_image("weight-stone", "weight")

def peak_flow_section():
    # TODO: get peak flow data
    return None

def transactions_section():
    """Incorporate the file of recent spending in monitored categories
    that is produced by chart-categories.lisp."""
    # TODO: spending per category per day of month/week

    spending_chart_file = os.path.join(FILECONF('general', 'charts'),
                                       "past-year.html")
    return T.div[wrap_box(
        linked_image("by-class", "transactions"),
        T.div[T.h3["Recent transactions"],
              recent_transactions_table(FILECONF('finance', 'main-account'), 14)],
        T.div[T.h3["Spending by category"],
              T.a(class_='plainlink', href="by-class.html")[
                  untemplate.safe_unicode(file_contents(spending_chart_file))]],
        T.div[T.h3["Automatic Spending by day of month"],
              untemplate.safe_unicode(file_contents(os.path.join(FILECONF('finance', 'merge-results-dir'),
                                                                 "auto-by-day-of-month.html")))],
        T.div[T.h3["Spending by day of week"],
              untemplate.safe_unicode(file_contents(os.path.join(FILECONF('finance', 'merge-results-dir'),
                                                                 "by-day-of-week.html")))],
        T.div[T.h3["Unmatched automatic transactions"],
              untemplate.safe_unicode(file_contents(os.path.join(FILECONF('finance', 'merge-results-dir'),
                                                                 "unmatched-auto.html")))],
        T.div[T.h3["Unmatched non-automatic transactions"],
              untemplate.safe_unicode(file_contents(os.path.join(FILECONF('finance', 'merge-results-dir'),
                                                                 "unmatched-non-auto.html")))])]

def timetable_section():
    day_after_tomorrow = qsutils.qsutils.forward_from(datetime.date.today(), None, None, 2)
    day_after_tomorrow_name = day_after_tomorrow.strftime("%A")
    return T.div(class_='timetable')[
        T.h2["Timetable"],
        switchable_panel(
            'timetable_switcher',
            {'today': one_day_timetable_section(with_form=True),
             'tomorrow': one_day_timetable_section(day=qsutils.qsutils.forward_from(datetime.date.today(),
                                                                                  None, None, 1)),
             day_after_tomorrow_name: one_day_timetable_section(day=day_after_tomorrow)},
            {'today': "Today",
             'tomorrow': "Tomorrow",
             day_after_tomorrow_name: day_after_tomorrow_name},
            ['today', 'tomorrow', day_after_tomorrow_name],
            'today')]

def one_day_timetable_section(day=None, with_form=False):
    # TODO: possibly add columns for weather data for the same times
    if day is None:
        day = datetime.date.today()
    day_of_week = day.strftime("%A")
    day_file = os.path.join(FILECONF('timetables', 'timetables-dir'), day_of_week + ".csv")
    extras = []
    if os.path.isfile(day_file):
        extras.append(day_file)
    with open(FILECONF('weather', 'weather-filename')) as weatherstream:
        weather = {row['time']: row for row in csv.DictReader(weatherstream)}
    # TODO: look up times in the weather
    # TODO: fetch from Google calendar (in update.py) and merge that in here
    table = T.table(id_="timetable")[
        T.caption["%s %s" % (day_of_week, day.isoformat())],
        [[T.tr(class_='inactive',
               name=slot.start.strftime("%H:%M"))[
                   T.td(class_='time_of_day')[slot.start.strftime("%H:%M")],
                   T.td(class_='activity')[T.a(href=slot.link)[slot.activity]
                                           if slot.link
                                           else slot.activity],
                   (T.td[T.input(type='checkbox',
                                 name=slot.activity, id_=slot.activity,
                                 class_='activity_logger')[""]]) if with_form else []]
          for slot in announce.get_day_announcer(
                  os.path.join(FILECONF('timetables', 'timetables-dir'),
                               CONF('timetables', 'default-timetable')),
                  extra_files=extras).ordered()]]]
    return T.form(action='log_done_timeslots')[
        table,
        T.input(type='submit',
                method='post',
                class_='activity_logger',
                value="Log activities")] if with_form else table

def weather_section():
    day_after_tomorrow = qsutils.qsutils.forward_from(datetime.date.today(), None, None, 2)
    day_after_tomorrow_name = day_after_tomorrow.strftime("%A")
    with open(CONF('weather', 'sunlight-times-file')) as sunlight_stream:
        sunlight_times = json.load(sunlight_stream)
    return T.div(class_='weather')[
        T.h2["Weather"],
        switchable_panel('weather_switcher',
                         {'today': one_day_weather_section(),
                          'tomorrow': one_day_weather_section(
                              qsutils.qsutils.forward_from(datetime.date.today(),
                                                         None, None, 1)),
                          # day_after_tomorrow_name: one_day_weather_section(
                          #     day_after_tomorrow)
                         },
                         {'today': "Today",
                          'tomorrow': "Tomorrow",
                          # day_after_tomorrow_name: day_after_tomorrow_name
                         },
                         ['today', 'tomorrow',
                          # day_after_tomorrow_name
                         ],
                         'today'),
        T.h3["Daylight times"],
        T.dl[T.dt["Sunrise:"], T.dd[sunlight_times['sunrise']],
        T.dt["Sunset:"], T.dd[sunlight_times['sunset']]]]

COMPASS_POINTS = ('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')

def compass_point_name(deg):
    return COMPASS_POINTS[int((int(deg) + (180 / len(COMPASS_POINTS))) // (360 / len(COMPASS_POINTS))) % len(COMPASS_POINTS)]

def one_day_weather_section(day=None):
    # https://pyowm.readthedocs.io/en/latest/v3/code-recipes.html
    if day is None:
        day = datetime.date.today()
    day_of_week = day.strftime("%A")
    daystring = day.isoformat()
    with open(FILECONF('weather', 'weather-filename')) as weatherstream:
        return T.table[
            T.caption["%s %s" % (day_of_week, daystring)],
            T.tr[T.th["Time"],
                 T.th["Temperature"],
                 T.th["Precipitation"],
                 T.th["Wind"],
                 T.th["Weather"]],
            [[T.tr(class_='inactive',
                   name=hour['time'][11:16])[
                       T.td(class_='weather weather_time')[hour['time'][11:19]],
                       T.td(class_='weather weather_temp')[str(round(float(hour['temperature']), 1))],
                       T.td(class_='weather weather_prec')[hour['precipitation']],
                       T.td(class_='weather weather_wind')[str(round(float(hour['wind-speed'])))
                            + " "
                            + compass_point_name(hour['wind-direction'])],
                       T.td(class_='weather weather_status')[hour['status']]]]
                        for hour in csv.DictReader(weatherstream)
                        if hour['time'].startswith(daystring)]]

def birthdays_section():
    people_by_id, _ = contacts_data.read_contacts(FILECONF('contacts', 'contacts-file'))
    today = datetime.date.today()
    this_year = today.year
    return T.table(class_='birthdays')[
        T.tr[T.th["Birthday"], T.th["Name"], T.th["Age"]],
        [T.tr[T.td(class_='birthday')[str(contacts_data.birthday(person, this_year))],
              T.td(class_='name')[make_name_with_email(contacts_data.make_name(person),
                                                       person.get('Primary email', ""))],
              T.td(class_='age')[contacts_data.age_string(person, this_year)]]
         for person in sorted([person
                              for person in people_by_id.values()
                              if contacts_data.birthday_soon(person, this_year, today, within_days=31)],
                              key=lambda person: contacts_data.birthday(person, this_year))]]

def keep_in_touch_section():
    """List people who I mean to keep in touch with but haven't for a while."""
    people_by_id, _ = contacts_data.read_contacts(FILECONF('contacts', 'contacts-file'))
    today = datetime.date.today()
    this_year = today.year
    long_uncontacted = [person
                        for person in people_by_id.values()
                        if contacts_data.contact_soon(person, today, days_since_last_contact=90)]
    if len(long_uncontacted) == 0:
        return T.p["No pending contacts."]
    return T.table(class_='contact_soon')[
        T.tr[T.th["Last contacted"], T.th["Name"]],
        [T.tr[T.td(class_='last_contacted')[str(contacts_data.last_contacted(person))],
              T.td(class_='name')[make_name_with_email(contacts_data.make_name(person),
                                                       person.get('Primary email', ""))]]
         for person in sorted(long_uncontacted,
                              key=lambda person: contacts_data.last_contacted(person))]]

def prayer_list_section():
    return None

def counts_table(caption, group):
    pairs = [(name, len(members)) for name, members in group.items()]
    s = sorted(pairs, key=lambda p: p[1])
    r = reversed(s)
    return T.div(class_='contacts_characteristics')[T.table[
        T.caption[caption],
        [T.tr[T.td[name], T.td[str(members)]]
         for name, members in r]]]

def contacts_section(contacts_analysis):
    if contacts_analysis is None:
        return None
    n_people = contacts_analysis['n_people']
    return T.dl[
        T.dt["Number of people"], T.dd[str(n_people)],
        T.dt["By gender"],
        T.dd["; ".join(["%s: %d" % (k, len(v))
                        for k, v in contacts_analysis['by_gender'].items()])],
        T.dt["Ordained"],
        T.dd["%d (%d%% of total)" % (contacts_analysis['ordained'],
                                     round(100*contacts_analysis['ordained']/n_people))],
        T.dt["Dr/Prof"],
        T.dd["%d (%d%% of total)" % (contacts_analysis['doctored'],
                                     round(100*contacts_analysis['doctored']/n_people))]]

def people_groups_section(contacts_analysis):
    if contacts_analysis is None:
        return None
    return row(counts_table("By nationality",
                            contacts_analysis['by_nationality']),
               counts_table("By title",
                            contacts_analysis['by_title']),
               counts_table("By place met",
                            contacts_analysis['by_place_met']))

DAYNAMES = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

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
                                                 T.td[DAYNAMES[row['Best before'].weekday()]],
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

def actions_section(from_org_mode):
    return wrap_box([T.h3["Mending"],
                     org_ql_list(top_items(from_org_mode["Mending"]))],
                    [T.h3["Physical making"],
                     org_ql_list(top_items(from_org_mode["Physical making"]))],
                    [T.h3["Programming"],
                     org_ql_list(top_items(from_org_mode["Programming"]))])

def org_ql_list(items):
    # TODO: make this scrollable
    return T.div(class_='agenda_list')[T.ul[[T.li[item['title']] for item in items]]]

def shopping_section(from_org_mode):
    return wrap_box([T.h3["Supermarket"],
                     org_ql_list(from_org_mode["Supermarket"])],
                    [T.h3["Online"],
                     org_ql_list(from_org_mode["Online"])])

def parcels_section():
    with open(FILECONF('dashboard', 'parcels')) as parcels_stream:
        parcels = json.load(parcels_stream)['expected']
    dates = {}
    for parcel in parcels:
        date = datetime.date.fromisoformat(parcel[0])
        if date not in dates:
            dates[date] = []
        dates[date].append(parcel[1])
    return [T.dl[[[T.dt[date.isoformat() + " " + DAYNAMES[date.weekday()]],
                   T.dd[T.ul[[[T.li[parcel]
                               for parcel in sorted(dates[date])]]]]]
                  for date in sorted(dates)]]]

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
    return T.div(class_='reflection')[
        T.p[random_reflection(reflections_dir)],
        T.p[random_reflection(reflections_dir)]]

def construct_dashboard_page(contacts_analysis):
    charts_dir = FILECONF('general', 'charts')
    page = SectionalPage()
    with open(FILECONF('dashboard', 'views')) as org_ql_stream:
        from_org_mode = json.load(org_ql_stream)
    page.add_section("Health", wrap_box(
        labelled_section("Weight", weight_section()),
        labelled_section("Calories", calories_section()),
        labelled_section("Meals", meals_section()),
        labelled_section("By day of week", calories_per_day_of_week()),
        labelled_section("Food groups", foods_section()),
        labelled_section("Running", running_section()),
        labelled_section("Cycling", cycling_section()),
        labelled_section("Blood pressure", blood_pressure_section()),
        labelled_section("Peak flow", peak_flow_section()),
        labelled_section("Sleep split", sleep_split_section()),
        labelled_section("Sleep times", sleep_times_section()),
        labelled_section("Sleep correlation", sleep_correlation_section()),
        labelled_section("Temperature", temperature_section())))
    page.add_section("Spending", transactions_section())
    page.add_section("People", wrap_box(
        labelled_section("Birthdays", birthdays_section()),
        labelled_section("To contact", keep_in_touch_section()),
        labelled_section("Prayer list", prayer_list_section()),
        labelled_section("People in contacts file", contacts_section(contacts_analysis)),
        labelled_section("People groups", people_groups_section(contacts_analysis))))
    page.add_section("Agenda", wrap_box(
        labelled_section("Actions", actions_section(from_org_mode)),
        labelled_section("Shopping", shopping_section((from_org_mode)))))
    page.add_section("Travel", travel_section())
    page.add_section("Inventory", inventory_section())
    page.add_section("Texts for reflection", reflection_section())
    return [T.body(onload="init_dashboard()")[
        T.script(src="dashboard.js"),
        T.h1["Personal dashboard"],
        wrap_box(T.div[page.toc(),
                       T.h2["Perishable food to use up"],
                       perishables_section(),
                       T.h2["Parcels expected"],
                       parcels_section()],
                 timetable_section(),
                 weather_section()),
        page.sections()]]

def page_text(page_contents, style_text, script_text):
    return untemplate.Serializer(untemplate.examples_vmap, 'utf-8').serialize(
        untemplate.HTML5Doc([untemplate.safe_unicode(style_text
                                                     + script_text),
                             page_contents],
                            head=T.head[T.meta(charset='utf-8'),
                                        T.meta(rel='stylesheet',
                                               type_='text/css',
                                               href="dashboard.css"),
                                        T.title["Personal dashboard"]]))

def file_contents(filename):
    if os.path.isfile(filename):
        with open(filename) as instream:
            return instream.read()
    return "File %s not found" % filename

def tagged(tag, text):
    return "<" + tag + ">" + text + "</" + tag + ">"

def tagged_file_contents(tag, filename):
    return tagged(tag, file_contents(filename))

def update_finances_charts(charts_dir, chart_sizes, begin_date, end_date, date_suffix, verbose):

    if not charts_dir:
        charts_dir = FILECONF('general', 'charts')
    qsutils.qschart.qscharts(os.path.join(charts_dir, "by-class.csv"),
                           'finances',
                           CATEGORIES_OF_INTEREST,
                           begin_date, end_date, None, False,
                           os.path.join(charts_dir, "by-class-%s-%%s.png" % date_suffix),
                           chart_sizes)
    # TODO: split main file into running balances for each account (tracking as needed), take the end of each month for each account, and put them all in a file to display here (and get that shown in the resulting page)
    # qsutils.qschart.qscharts(FILECONF('finance', 'account-balances'), 'finances',
    #                        [FILECONF('finance', 'main-current-account'),
    #                         FILECONF('finance', 'main-savings-account')],
    #                        begin, end, None, False,
    #                        os.path.join(charts_dir, "balances-%s-%%s.png" % date_suffix),
    #                        chart_sizes)

def update_physical_charts(charts_dir, chart_sizes, begin_date, end_date, date_suffix):

    if not charts_dir:
        charts_dir = FILECONF('general', 'charts')
    physical = FILECONF('physical', 'physical-filename')
    mfp_filename = FILECONF('physical', 'mfp-filename')

    split_by_DoW = False

    # TODO: rolling averages
    for units in ('stone', 'kilogram', 'pound'):
        qsutils.qschart.qscharts(physical,
                               'weight',
                               [units],
                               begin_date, end_date, None, split_by_DoW,
                               os.path.join(charts_dir, "weight-%s-%s-%%s.png" % (units, date_suffix)),
                               chart_sizes)

    for chartdef, template in [
            ({'mainfile': mfp_filename,
              'file_type': 'calories',
              'columns': ['calories']},
             "total_calories-%s-%%s.png"),
            ({'mainfile': mfp_filename,
              'file_type': 'meals',
              'columns': ['breakfast', 'lunch', 'dinner', 'snacks']},
             "meal_calories-%s-%%s.png"),
            ({'mainfile': mfp_filename,
              'file_type': 'food_groups',
              'columns': ['carbohydrates', 'fat', 'protein', 'sugar']
              }, "origin_calories-%s-%%s.png"),
            # ({'mainfile': FILECONF('physical', 'oura-filename'),
            #   'file_type': 'sleep',
            #   'columns': ['Latency', 'Rem', 'Deep', 'Total']
            #   }, "sleep-split-%s-%%s.png"),
            ({'mainfile': FILECONF('physical', 'omron-filename'),
              'file_type': 'blood_pressure',
              'columns': ['systolic', 'diastolic', 'heart_rate']
              }, "blood-pressure-%s-%%s.png"),
            ({'mainfile': FILECONF('physical', 'cycling-filename'),
              'file_type': 'cycling',
              'columns': ['Distance', 'Calories', 'Time']
              }, "cycling-%s-%%s.png"),
            ({'mainfile': FILECONF('physical', 'running-filename'),
              'file_type': 'running',
              'columns': ['Distance', 'Calories', 'Time']
              }, "running-%s-%%s.png"),
            # ({'mainfile':,
            #   'file_type':,
            #   'columns':
            #   }, ),
            # ({'mainfile':,
            #   'file_type':,
            #   'columns':
            #   }, ),
            # ({'mainfile':,
            #   'file_type':,
            #   'columns':
            #   }, ),
            # ({'mainfile':,
            #   'file_type':,
            #   'columns':
            #   }, ),
            # ({'mainfile':,
            #   'file_type':,
            #   'columns':
            #   }, )
    ]:
        qsutils.qschart.qscharts(begin=begin_date, end=end_date,
                                 match=None,
                                 by_day_of_week=split_by_DoW,
                                 plot_param_sets=chart_sizes,
                                 outfile_template=os.path.join(charts_dir,
                                                               template % date_suffix),
                                 bar=False,
                                 **chartdef)

    sleep_chart_params = {suffix: chart.copy() for suffix, chart in chart_sizes.items()}
    for scp in sleep_chart_params.values():
        scp['subplot_kw'] = {'ylim': (0, 24.0)}
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
                         contacts_analysis,
                         details_background_color="gold", inline=True):
    """Construct and save the dashboard page."""
    if not charts_dir:
        charts_dir = FILECONF('general', 'charts')
    with open(os.path.join(charts_dir, "index.html"), 'w') as page_stream:
        page_stream.write(
            page_text(
                construct_dashboard_page(contacts_analysis),
                ((tagged_file_contents("style", os.path.join(source_dir, "dashboard.css"))
                 + qsutils.qsutils.table_support_css(details_background_color))
                 if inline
                 else ""),
                (tagged_file_contents("script", os.path.join(source_dir, "dashboard.js"))
                 if inline
                 else "")))
    if not inline:
        for filename in ("dashboard.css",
                         "dashboard.js"):
            shutil.copy(os.path.join(source_dir, filename),
                        os.path.join(charts_dir, filename))

def make_dashboard_images(charts_dir,
                          chart_sizes,
                          background_colour,
                          begin_date=None, end_date=None,
                          verbose=False):
    """Make all the images for the dashboard."""
    today = datetime.date.today()
    for param_set in chart_sizes.values():
        param_set['facecolor'] = background_colour

    periods = {'all_time': datetime.date(year=1973, month=1, day=1),
               'past_week': qsutils.qsutils.back_from(today, None, None, 7),
               'past_month': qsutils.qsutils.back_from(today, None, 1, None),
               'past_quarter': qsutils.qsutils.back_from(today, None, 3, None),
               'past_year': qsutils.qsutils.back_from(today, 1, None, None)}
    for date_suffix, begin in ({'custom': begin_date}
                               if begin_date
                               else periods).items():
        begin = np.datetime64(datetime.datetime.combine(begin, datetime.time())) # .timestamp()
        update_finances_charts(charts_dir, chart_sizes, begin, end_date, date_suffix, verbose)
        update_physical_charts(charts_dir, chart_sizes, begin, end_date, date_suffix)

def make_dashboard_page(charts_dir=None,
                        contacts_analysis=None,
                        chart_sizes={'small': {'figsize': (5,4)},
                                     'large': {'figsize': (11,8)}},
                        begin_date=None, end_date=None,
                        verbose=False):

    """Make the dashboard page, including refreshed images for it."""

    global CONFIGURATION
    CONFIGURATION = lifehacking_config.load_config()

    if not charts_dir:
        charts_dir = CONFIGURATION['general']['charts']

    text_colour, background_colour, shading = dashboard_page_colours()
    make_dashboard_images(charts_dir,
                          chart_sizes,
                          background_colour,
                          begin_date, end_date,
                          verbose)
    write_dashboard_page(charts_dir,
                         contacts_analysis,
                         details_background_color=shading)

def main():
    parser = qsutils.qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/private_html/dashboard"),
                        help="""Directory to write charts into.""")
    args = parser.parse_args()

    make_dashboard_page(charts_dir=args.charts)

if __name__ == '__main__':
    main()
