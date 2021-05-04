#!/usr/bin/python3

import csv
import datetime
import glob
import os
import random
import shutil
import sys

sys.path.append(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import financial.classify
import utils.qsutils

my_projects = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
sys.path.append(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

sys.path.append(os.path.join(my_projects, "coimealta/contacts"))
import contacts_data

sys.path.append(os.path.join(my_projects, "noticeboard"))

import announce

sys.path.append(os.path.join(my_projects, "coimealta/inventory"))
import perishables

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

def make_remaining_cell(thresholds, spent_this_month, coi):
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

class SectionalPage(object):
    pass

    def __init__(self):
        self._sections = []

    def add_section(self, title, body):
        self._sections.append((title, body))

    def toc(self):
        return [T.h2["Table of contents"],
                T.ul[[T.li[T.a(href="#"+namify(section[0]))[section[0]]] for section in self._sections]]]

    def sections(self):
        return [[T.div(class_="section")[T.h2[T.a(name=namify(section[0]))[section[0]]],
                       T.div(class_="sectionbody")[section[1]]] for section in self._sections]]

def linked_image(image_name, label):
    return T.div(class_='imageswitcher', id=label)[
        [T.div(class_="%s" % period)[T.a(href="%s-%s-large.png" % (image_name, period))[
            # TODO: use thumb image, with small image revealed on hover, keeeping the large one linked
            T.img(src="%s-%s-small.png" % (image_name, period))]] for period in ('all_time', 'past_year', 'past_quarter', 'past_month', 'past_week')],
        T.form[T.button(class_='inactive', onclick='setperiod("%s", "all_time")'%label)['all'],
               T.button(class_='inactive', onclick='setperiod("%s", "past_year")'%label)['year'],
               T.button(class_='active', onclick='setperiod("%s", "past_quarter")'%label)['quarter'],
               T.button(class_='inactive', onclick='setperiod("%s", "past_month")'%label)['month'],
               T.button(class_='inactive', onclick='setperiod("%s", "past_week")'%label)['week']]]

def weight_section():
    return linked_image("weight-stone", "weight")

def transactions_section():
    return T.div[T.p["Full details ", T.a(href="by-class.html")["here"], "."],
                 linked_image("by-class", "transactions")]

def timetable_section():
    return T.div[T.table[
        T.h2["Timetable for %s" % datetime.date.today().isoformat()],
        [[T.tr[T.td[slot.start.strftime("%H:%M")], T.td[slot.activity]]
          for slot in announce.get_day_announcer(os.path.expandvars("$COMMON/timetables/timetable.csv")).ordered()]]]]

def budget_section(config, charts_dir):
    thresholds = financial.classify.read_thresholds(config, "budgetting-thresholds.yaml")
    with open(os.path.join(charts_dir, "by-class-this-year.csv")) as spent_stream:
        spent = [row for row in csv.DictReader(spent_stream)]
        spent_month_before_last = spent[-3] if len(spent) >= 3 else None
        spent_last_month = spent[-2] if len(spent) >= 2 else None
        spent_this_month = spent[-1]
        return T.table(class_='budgetting')[
            T.tr[[T.th[""]] + [T.th[coi] for coi in CATEGORIES_OF_INTEREST]],
            T.tr(class_='monthly_budget')[[T.th["Monthly budget"]] + [T.td(class_='budget')[str(thresholds[coi])] for coi in CATEGORIES_OF_INTEREST]],
            (T.tr(class_='spent_month_before_last')[[T.th["Spent month before last"]] + [T.td(class_='spent')[str(spent_month_before_last[coi])] for coi in CATEGORIES_OF_INTEREST]]) if spent_month_before_last else [], # TODO: style according to whether over threshold
            (T.tr(class_='spent_last_month')[[T.th["Spent last month"]] + [T.td(class_='spent')[str(spent_last_month[coi])] for coi in CATEGORIES_OF_INTEREST]]) if spent_last_month else [], # TODO: style according to whether over threshold
            T.tr(class_='spent_this_month')[[T.th["Spent this month"]] + [T.td(class_='spent')[str(spent_this_month[coi])] for coi in CATEGORIES_OF_INTEREST]], # TODO: style according to whether over threshold
            T.tr(class_='remaining_this_month')[[T.th["Remaining this month"]] + [make_remaining_cell(thresholds, spent_this_month, coi) for coi in CATEGORIES_OF_INTEREST]]]

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
                              if contacts_data.birthday_soon(person, this_year, today)],
                              key=lambda person: contacts_data.birthday(person, this_year))]]

def contact_section():
    people_by_id, _ = contacts_data.read_contacts(os.path.expandvars("$COMMON/org/contacts.csv"))
    today = datetime.date.today()
    this_year = today.year
    return T.table(class_='contact_soon')[
        T.tr[T.th["Last contacted"], T.th["Name"]],
        [T.tr[T.td(class_='last_contacted')[str(contacts_data.last_contacted(person))],
              T.td(class_='name')[make_name_with_email(contacts_data.make_name(person),
                                                       person.get('Primary email', ""))]]
         for person in sorted([person
                              for person in people_by_id.values()
                              if contacts_data.contact_soon(person, today)],
                              key=lambda person: contacts_data.last_contacted(person))]]

def perishables_section():
    return T.table[[T.tr[T.td[row['Best before'].isoformat()],
                         T.td[row['Product']],
                         T.td[str(row['Quantity'])]]
        for row in perishables.get_perishables()]]

def diet_section():
    return T.div(class_="dietary")[
        row(T.div[T.h3["Calories by meal"],
                  linked_image("meal_calories", "meal_calories")],
            T.div[T.h3("Food groups"),
                  linked_image("origin_calories", "origins")])]

def random_reflection():
    reflections_dir = os.path.expandvars("$COMMON/texts/reflection")
    with open(random.choice(glob.glob(os.path.join(reflections_dir, "*.txt")))) as instream:
        return random.choice([line.strip() for line in instream if line != "\n"])

def reflection_section():
    return T.div(class_="reflection")[
        T.p[random_reflection()],
        T.p[random_reflection()]]

def construct_dashboard_page(config, charts_dir):
    page = SectionalPage()
    page.add_section("Weight", weight_section())
    # page.add_section("Exercise", T.p["placeholder for exercise data from MFP"])
    # page.add_section("Sleep", T.p["placeholder for sleep data from Oura"])
    page.add_section("Diet", diet_section())
    page.add_section("Spending", transactions_section())
    # TODO: insert recent transaction chart from file
    # page.add_section("Spending", untemplate.safe_unicode(file_contents(spending_chart_file)))
    page.add_section("Monthly budgets", budget_section(config, charts_dir))
    page.add_section("Contacting people", row(birthdays_section(), contact_section()))
    page.add_section("Food to use up in fridge", perishables_section())
    # page.add_section("Actions", T.p["placeholder for data from org-ql"])
    # page.add_section("Shopping", T.p["placeholder for data from org-ql"])
    # page.add_section("Travel", T.p["placeholder for google movement tracking"])
    page.add_section("Text for reflection", reflection_section())
    return [T.body[
        T.script(src="dashboard.js"),
        T.h1["My dashboard"],
        row(page.toc(), T.div(class_="timetable")[timetable_section()]),
        page.sections()]]

def page_text(page_contents, style_text, script_text):
    return untemplate.Serializer(untemplate.examples_vmap, 'utf-8').serialize(
        untemplate.HTML5Doc([untemplate.safe_unicode(style_text
                                                     + script_text),
                             page_contents],
                            head=T.head[T.meta(rel='stylesheet',
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

def write_dashboard_page(config, charts_dir, inline=True):
    source_dir = os.path.dirname(os.path.realpath(__file__))
    with open(os.path.join(charts_dir, "index.html"), 'w') as page_stream:
        page_stream.write(
            page_text(
                construct_dashboard_page(config, charts_dir),
                tagged_file_contents("style", os.path.join(source_dir, "dashboard.css")) if inline else "",
                tagged_file_contents("script", os.path.join(source_dir, "dashboard.js")) if inline else ""))
    if not inline:
        for filename in ("dashboard.css", "dashboard.js"):
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
