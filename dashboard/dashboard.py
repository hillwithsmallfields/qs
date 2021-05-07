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
    return T.td(class_="ok" if spent <= available else "overspent")[str(available + spent)]

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
        return [[T.div(class_="section")[T.h2[T.a(name=namify(section[0]))[section[0]]],
                       T.div(class_="sectionbody")[section[1]]] for section in self._sections]]

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
    return T.div(class_='imageswitcher', id=label)[
        [T.div(class_="%s" % period)[T.a(href="%s-%s-large.png" % (image_name, period))[
            # TODO: use thumb image, with small image revealed on hover, keeeping the large one linked
            T.img(src="%s-%s-small.png" % (image_name, period))]] for period in ('all_time', 'past_year', 'past_quarter', 'past_month', 'past_week')],
        T.form[T.button(class_='inactive', onclick="setperiod('%s', 'all_time')"%label)['all'],
               T.button(class_='inactive', onclick="setperiod('%s', 'past_year')"%label)['year'],
               T.button(class_='active', onclick="setperiod('%s', 'past_quarter')"%label)['quarter'],
               T.button(class_='inactive', onclick="setperiod('%s', 'past_month')"%label)['month'],
               T.button(class_='inactive', onclick="setperiod('%s', 'past_week')"%label)['week']]]

def weight_section():
    return linked_image("weight-stone", "weight")

def transactions_section(charts_dir):
    """Incorporate the file of recent spending in monitored categories
    that is produced by chart-categories.lisp."""
    spending_chart_file = os.path.join(charts_dir, "past-quarter.html")
    return T.div[T.p["Full details ", T.a(href="by-class.html")["here"], "."],
                 wrap_box(linked_image("by-class", "transactions"),
                          untemplate.safe_unicode(file_contents(spending_chart_file)))]

def timetable_section():
    # TODO: possibly add columns for weather data for the same times
    # TODO: add script to change the row class for the row containing the current time
    today = datetime.date.today()
    day_of_week = today.strftime("%A")
    day_file = os.path.join(os.path.expandvars("$COMMON/timetables"), day_of_week + ".csv")
    extras = []
    if os.path.isfile(day_file):
        # TODO: debug this merge, I don't think it's right yet
        extras.append(day_file)
    # TODO: fetch from Google calendar
    return T.div[T.table[
        T.h2["Timetable for %s %s" % (day_of_week, today.isoformat())],
        [[T.tr(class_="inactive")[T.td[slot.start.strftime("%H:%M")],
                                  T.td[T.a(href=slot.link)[slot.activity]
                                       if slot.link
                                       else slot.activity]]
          for slot in announce.get_day_announcer(
                  os.path.expandvars("$COMMON/timetables/timetable.csv"),
                  extra_files=extras).ordered()]]]]

def birthdays_section():
    people_by_id, _ = contacts_data.read_contacts(os.path.expandvars("$COMMON/org/contacts.csv"))
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
    people_by_id, _ = contacts_data.read_contacts(os.path.expandvars("$COMMON/org/contacts.csv"))
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
    return T.div(class_="contacts_characteristics")[T.table[
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
        T.dd["%d (%g of total)" % (contacts_analysis['ordained'],
                                   contacts_analysis['ordained']/n_people)],
        T.dt["Dr/Prof"],
        T.dd["%d (%g of total)" % (contacts_analysis['doctored'],
                                   contacts_analysis['doctored']/n_people)]]

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
    return (T.p["No items on record."]
            if len(items) == 0
            else T.table[[T.tr[T.td[row['Best before'].isoformat()],
                               T.td[row['Product']],
                               T.td[str(row['Quantity'])]]
                          for row in items]])

def calories_section():
    return linked_image("total_calories", "total_calories")

def meals_section():
    return linked_image("meal_calories", "meal_calories")

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

def sleep_section():
    # TODO: read from data fetched from Oura (sleep.csv)
    return None

def actions_section():
    # TODO: use org-ql to produce a filee
    return None

def shopping_section():
    # TODO: use org-ql to produce a filee
    return None

def inventory_section():
    # TODO: use coimealta/inventory
    return None

def travel_section():
    # TODO: read travel.csv and a journeys file generated from Google
    return None

def random_reflection():
    reflections_dir = os.path.expandvars("$COMMON/texts/reflection")
    with open(random.choice(glob.glob(os.path.join(reflections_dir, "*.txt")))) as instream:
        return random.choice([line.strip() for line in instream if line != "\n"])

def reflection_section():
    return T.div(class_="reflection")[
        T.p[random_reflection()],
        T.p[random_reflection()]]

def construct_dashboard_page(config, charts_dir, contacts_analysis):
    page = SectionalPage()
    page.add_section("Weather", weather_section())
    page.add_section("Health", wrap_box(
        labelled_section("Weight", weight_section()),
        labelled_section("Calories", calories_section()),
        labelled_section("Meals", meals_section()),
        labelled_section("Food groups", foods_section()),
        labelled_section("Exercise", exercise_section()),
        labelled_section("Sleep", sleep_section())))
    page.add_section("Spending", transactions_section(charts_dir))
    page.add_section("People", wrap_box(
        labelled_section("Birthdays", birthdays_section()),
        labelled_section("To contact", keep_in_touch_section()),
        labelled_section("People", contacts_section(contacts_analysis)),
        labelled_section("People groups", people_groups_section(contacts_analysis))))
    page.add_section("Food to use up in fridge", perishables_section())
    page.add_section("Agenda", wrap_box(
        labelled_section("Actions", actions_section()),
        labelled_section("Shopping", shopping_section())))
    page.add_section("Travel", travel_section())
    page.add_section("Inventory", inventory_section())
    page.add_section("Texts for reflection", reflection_section())
    return [T.body[
        T.script(src="dashboard.js"),
        T.h1["My dashboard"],
        wrap_box(page.toc(),
                 T.div(class_="timetable")[timetable_section()]),
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
                                        T.title["Dashboard"]]))

def file_contents(filename):
    with open(filename) as instream:
        return instream.read()

def tagged(tag, text):
    return "<" + tag + ">" + text + "</" + tag + ">"

def tagged_file_contents(tag, filename):
    return tagged(tag, file_contents(filename))

def write_dashboard_page(config, charts_dir, contacts_analysis, details_background_color="gold", inline=True):
    with open(os.path.join(charts_dir, "index.html"), 'w') as page_stream:
        page_stream.write(
            page_text(
                construct_dashboard_page(config, charts_dir, contacts_analysis),
                (tagged_file_contents("style", os.path.join(source_dir, "dashboard.css"))
                 + utils.qsutils.table_support_css(details_background_color)) if inline else "",
                tagged_file_contents("script", os.path.join(source_dir, "dashboard.js")) if inline else ""))
    if not inline:
        for filename in ("dashboard.css",
                         # TODO: put table_support_css into a file and add it here
                         "dashboard.js"):
            shutil.copy(os.path.join(source_dir, filename),
                        os.path.join(charts_dir, filename))

def main():
    parser = utils.qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/private_html/dashboard"),
                        help="""Directory to write charts into.""")
    args = parser.parse_args()

    configdir = os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/conf")
    config = utils.qsutils.load_config(args.verbose, None, None, accounts_config, conversions_config)
    write_dashboard_page(config, args.charts)

if __name__ == '__main__':
    main()
