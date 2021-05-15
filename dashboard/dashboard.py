#!/usr/bin/python3

import cssutils
import csv
import datetime
import decouple
import glob
import os
import pyowm
import random
import shutil
import sys

source_dir = os.path.dirname(os.path.realpath(__file__))

# other parts of this project group
sys.path.append(os.path.dirname(source_dir))
import financial.classify       # https://github.com/hillwithsmallfields/qs/blob/master/financial/classify.py
import utils.qsutils            # https://github.com/hillwithsmallfields/qs/blob/master/utils/qsutils.py

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

sys.path.append(os.path.join(my_projects, "coimealta/inventory"))
import perishables              # https://github.com/hillwithsmallfields/coimealta/blob/master/inventory/perishables.py

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

def linked_image(image_name, label):
    """Return a collection of images wrapped in switcher,
    with each image linked to a larger version."""
    return T.table(class_='switcher', id_=label)[
        T.tr(align="center")[T.td[[T.div(class_='choice', name=period)[T.a(href="%s-%s-large.png" % (image_name, period))[
            T.img(src="%s-%s-small.png" % (image_name, period))]] for period in ('all_time', 'past_year', 'past_quarter', 'past_month', 'past_week')]]],
        T.tr(align="center")[
            T.td[T.div[T.button(class_='inactive', name='all_time', onclick="select_version('%s', 'all_time')"%label)['all'],
                       T.button(class_='inactive', name='past_year', onclick="select_version('%s', 'past_year')"%label)['year'],
                       T.button(class_='active', name='past_quarter', onclick="select_version('%s', 'past_quarter')"%label)['quarter'],
                       T.button(class_='inactive', name='past_month', onclick="select_version('%s', 'past_month')"%label)['month'],
                       T.button(class_='inactive', name='past_week', onclick="select_version('%s', 'past_week')"%label)['week']]]]]

def weight_section():
    return linked_image("weight-stone", "weight")

def peak_flow_section():
    # TODO: get peak flow data
    return None

def transactions_section(file_locations):
    """Incorporate the file of recent spending in monitored categories
    that is produced by chart-categories.lisp."""
    # TODO: spending per category per day of month/week
    spending_chart_file = os.path.join(file_locations['charts'], "past-quarter.html")
    return T.div[wrap_box(linked_image("by-class", "transactions"),
                          # I'd like to do this, but keeping the maroon colours
                          T.a(class_='plainlink', href="by-class.html")[
                              untemplate.safe_unicode(file_contents(spending_chart_file))])]

def timetable_section(file_locations):
    # TODO: possibly add columns for weather data for the same times
    today = datetime.date.today()
    day_of_week = today.strftime("%A")
    day_file = os.path.join(file_locations['timetables-dir'], day_of_week + ".csv")
    extras = []
    if os.path.isfile(day_file):
        # TODO: debug this merge, I don't think it's right yet
        extras.append(day_file)
    # TODO: fetch from Google calendar
    return T.div[T.h2["Timetable for %s %s" % (day_of_week, today.isoformat())],
                 T.table(id_="timetable")[
                     [[T.tr(class_='inactive',
                            name=slot.start.strftime("%H:%M"))[
                                T.td(class_='time_of_day')[slot.start.strftime("%H:%M")],
                                T.td(class_='activity')[T.a(href=slot.link)[slot.activity]
                                     if slot.link
                                     else slot.activity]]
          for slot in announce.get_day_announcer(
                  os.path.join(file_locations['timetables-dir'],
                               file_locations['default-timetable']),
                  extra_files=extras).ordered()]]]]

def birthdays_section(file_locations):
    people_by_id, _ = contacts_data.read_contacts(file_locations['contacts-file'])
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

def keep_in_touch_section(file_locations):
    """List people who I mean to keep in touch with but haven't for a while."""
    people_by_id, _ = contacts_data.read_contacts(file_locations['contacts-file'])
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

def counts_table(caption, group):
    pairs = [(name, len(members)) for name, members in group.items()]
    s = sorted(pairs, key=lambda p: p[1])
    r = reversed(s)
    return T.div(class_='contacts_characteristics')[T.table[
        T.caption[caption],
        [T.tr[T.td[name], T.td[str(members)]]
         for name, members in r]]]

def contacts_section(contacts_analysis):
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
    return row(counts_table("By nationality",
                            contacts_analysis['by_nationality']),
               counts_table("By title",
                            contacts_analysis['by_title']),
               counts_table("By place met",
                            contacts_analysis['by_place_met']))

def perishables_section():
    """List things to use up from the fridge, in order of expiry date."""
    items = perishables.get_perishables()
    week_ahead = (datetime.datetime.now() + datetime.timedelta(days=7)).date()
    return (T.p["No items on record."]
            if len(items) == 0
            else T.table[[T.tr(class_="use_soon"
                               if row['Best before'] < week_ahead
                               else "use_later")[T.td[row['Best before'].isoformat()],
                               T.td[row['Product']],
                               T.td[str(row['Quantity'])]]
                          for row in items]])

def calories_section():
    return linked_image("total_calories", "total_calories")

def meals_section():
    return linked_image("meal_calories", "meal_calories")

def calories_per_day_of_week():
    # TODO: make file with calories split by day of week, and display that
    return None

def foods_section():
    return linked_image("origin_calories", "origins")

def weather_section():
    # https://pyowm.readthedocs.io/en/latest/v3/code-recipes.html
    owm = pyowm.owm.OWM(decouple.config('OWM_API_KEY'))
    reg = owm.city_id_registry()
    list_of_locations = reg.locations_for('cambridge', country='GB')
    cambridge = list_of_locations[0]
    mgr = owm.weather_manager()
    one_call = mgr.one_call(lat=cambridge.lat, lon=cambridge.lon)
    three_h_forecast = mgr.forecast_at_place('Cambridge,GB', '3h').forecast
    print("three_h_forecast is", three_h_forecast)
    temp = one_call.forecast_daily[0].temperature('celsius').get('feels_like_morn', None)
    # TODO: pick out the data I want
    print("temp is", temp)
    print("one_call is", one_call)
    return None

def exercise_section():
    # TODO: fetch from MFP and Garmin
    return None

def sleep_split_section():
    return linked_image("sleep-split", "sleep-split")

def sleep_times_section():
    return linked_image("sleep-times", "sleep-times")

def blood_pressure_section():
    return None

def temperature_section():
    return linked_image("temperature", "temperature")

def actions_section(file_locations):
    # TODO: use org-ql to produce a file
    return None

def shopping_section(file_locations):
    # TODO: use org-ql to produce a file
    return None

def items_table(items):
    items_by_type = {}
    for item in items.values():
        key = "%s (%s)" % (item['Type'], item['Subtype'])
        if key in items_by_type:
            items_by_type[key].append(item)
        else:
            items_by_type[key] = [item]
    return T.div(class_='inventory_list')[T.table[[T.tr[T.th[key], T.td[len(items_by_type[key])]]
                                                   for key in sorted(items_by_type.keys(),
                                                                     reverse=True,
                                                                     key=lambda k: len(items_by_type[k]))]]]

def inventory_section(file_locations):

    locations = storage.read_locations(file_locations['storage-file'])
    items = storage.read_inventory(file_locations['inventory-file'])
    stock = storage.read_inventory(file_locations['stock-file'])
    project_parts = storage.read_inventory(file_locations['project-parts-file'])
    media = storage.read_books(file_locations['books-file'])

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
                                             T.dd[str(len(media_by_type[mtype]))]] for mtype in sorted(media_by_type)]],
                                 T.p["Your most recent book acquisition was of ", latest_book['Title'],
                                     " on ", latest_book['Acquired'], "."]]],
                       T.div[T.h3["General possessions"], items_table(items)],
                       T.div[T.h3["Project parts"], items_table(project_parts)],
                       T.div[T.h3["Stock"], items_table(stock)],
                       T.div[T.h3["Storage"],
                             T.div(class_='inventory_list')[T.dl[T.dt["Container volume"], T.dd["%g litres" % volume],
                                                                 T.dt["Bookshelf length"], T.dd["%g metres" % bookshelf_length],
                                                                 T.dt["Other shelf length"], T.dd["%g metres" % other_length]]]])]]

def travel_section(file_locations):
    # TODO: read travel.csv and a journeys file generated from Google
    return None

def random_reflection(reflections_dir):
    with open(random.choice(glob.glob(os.path.join(reflections_dir, "*.txt")))) as instream:
        return random.choice([line.strip() for line in instream if line != "\n"])

def reflection_section(file_locations):
    reflections_dir = file_locations['reflections-dir']
    return T.div(class_='reflection')[
        T.p[random_reflection(reflections_dir)],
        T.p[random_reflection(reflections_dir)]]

def construct_dashboard_page(file_locations,
                             contacts_analysis,
                             finance_updates_analysis):
    charts_dir = file_locations['charts']
    page = SectionalPage()
    page.add_section("Perishable food to use up", perishables_section())
    page.add_section("Weather", weather_section())
    page.add_section("Health", wrap_box(
        labelled_section("Weight", weight_section()),
        labelled_section("Calories", calories_section()),
        labelled_section("Meals", meals_section()),
        labelled_section("By day of week", calories_per_day_of_week()),
        labelled_section("Food groups", foods_section()),
        labelled_section("Exercise", exercise_section()),
        labelled_section("Blood pressure", blood_pressure_section()),
        labelled_section("Peak flow", peak_flow_section()),
        labelled_section("Sleep split", sleep_split_section()),
        labelled_section("Sleep times", sleep_times_section()),
        labelled_section("Temperature", temperature_section())))
    page.add_section("Spending", transactions_section(file_locations))
    page.add_section("Recent update debug", T.pre[finance_updates_analysis])
    page.add_section("People", wrap_box(
        labelled_section("Birthdays", birthdays_section(file_locations)),
        labelled_section("To contact", keep_in_touch_section(file_locations)),
        labelled_section("People in contacts file", contacts_section(contacts_analysis)),
        labelled_section("People groups", people_groups_section(contacts_analysis))))
    page.add_section("Agenda", wrap_box(
        labelled_section("Actions", actions_section(file_locations)),
        labelled_section("Shopping", shopping_section(file_locations))))
    page.add_section("Travel", travel_section(file_locations))
    page.add_section("Inventory", inventory_section(file_locations))
    page.add_section("Texts for reflection", reflection_section(file_locations))
    return [T.body(onload="start_timetable_updater()")[
        T.script(src="dashboard.js"),
        T.h1["Personal dashboard"],
        wrap_box(page.toc(),
                 T.div(class_='timetable')[timetable_section(file_locations)]),
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
    with open(filename) as instream:
        return instream.read()

def tagged(tag, text):
    return "<" + tag + ">" + text + "</" + tag + ">"

def tagged_file_contents(tag, filename):
    return tagged(tag, file_contents(filename))

def write_dashboard_page(file_locations,
                         contacts_analysis,
                         finance_updates_analysis,
                         details_background_color="gold", inline=True):
    charts_dir = file_locations['charts']
    with open(os.path.join(charts_dir, "index.html"), 'w') as page_stream:
        page_stream.write(
            page_text(
                construct_dashboard_page(file_locations, contacts_analysis, finance_updates_analysis),
                (tagged_file_contents("style", os.path.join(source_dir, "dashboard.css"))
                 + utils.qsutils.table_support_css(details_background_color)) if inline else "",
                tagged_file_contents("script", os.path.join(source_dir, "dashboard.js")) if inline else ""))
    if not inline:
        for filename in ("dashboard.css",
                         "dashboard.js"):
            shutil.copy(os.path.join(source_dir, filename),
                        os.path.join(charts_dir, filename))

def make_dashboard_images(file_locations):
    today = datetime.date.today()
    text_colour, background_colour, shading = dashboard_page_colours()
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

def make_dashboard_page(file_locations, contacts_analysis, finance_updates_analysis):
    make_dashboard_images(file_locations)
    write_dashboard_page(file_locations,
                         contacts_analysis,
                         finance_updates_analysis,
                         details_background_color=shading)

def main():
    parser = utils.qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/private_html/dashboard"),
                        help="""Directory to write charts into.""")
    args = parser.parse_args()

    file_locations = {'charts', args.chart}

    make_dashboard_page(file_locations)

if __name__ == '__main__':
    main()
