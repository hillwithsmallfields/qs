#!/usr/bin/python3

import argparse
import csv
import datetime
import glob
import os
import random
import re
import shutil
import sys
import yaml

import check_merged_row_dates
import classify
import finlisp
import list_completions
import mfp_reader
import qschart
import qsmerge
import qsutils

my_projects = os.path.dirname(os.path.dirname(sys.path[0]))
sys.path.append(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

sys.path.append(os.path.join(my_projects, "coimealta/contacts"))
import link_contacts
import contacts_data

sys.path.append(os.path.join(my_projects, "coimealta/inventory"))
import perishables

sys.path.append(os.path.join(my_projects, "noticeboard"))

import announce

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

DASHBOARD_STYLESHEET = """
<style>
td.ok {
    background-color: green;
}
td.overspent {
    background-color: red;
}
</style>
"""

def make_remaining_cell(thresholds, spent_this_month, coi):
    available = float(thresholds.get(coi, 0))
    spent_string = spent_this_month.get(coi, "")
    spent = 0 if spent_string == "" else float(spent_string)
    return T.td(class_="ok" if spent <= available else "overspent")[str(available + spent)]

def birthday(person, this_year):
    bday_string = person.get('Birthday', "") or ""
    if bday_string == "":
        return False
    try:
        bday = datetime.date.fromisoformat(bday_string)
    except Exception:
        match = re.search("-([0-9][0-9])-([0-9][0-9])", bday_string)
        if match:
            month = int(match.group(1))
            if month == 0:
                return False
            day = int(match.group(2))
            if day == 0:
                return False
            bday = datetime.date(year=this_year, month=month, day=day)
        else:
            return False
    bday = bday.replace(year=this_year)
    return bday

def birthday_soon(person, this_year, today):
    bday = birthday(person, this_year)
    if not bday:
        return False
    interval_to_birthday = (bday - today).days
    return interval_to_birthday > 0 and interval_to_birthday < 30

def last_contacted(person):
    cday_string = person.get('In touch', "")
    if not cday_string:
        return None
    try:
        cday = datetime.date.fromisoformat(cday_string)
    except:
        match = re.search("([0-9][0-9][0-9][0-9])-([0-9][0-9])", cday_string)
        if match:
            year = int(match.group(1))
            if year == 0:
                return False
            month = int(match.group(2))
            if month == 0:
                month = 1
            cday = datetime.date(year=year, month=month, day=1)
        else:
            match = re.search("([0-9][0-9][0-9][0-9])", cday_string)
            if match:
                cday = datetime.date(year=int(match.group(1)), month=1, day=1)
            else:
                return None
    return cday

def contact_soon(person, today):
    cday = last_contacted(person)
    return cday and (today - cday).days > 45

def age_string(person, year):
    age = contacts_data.age_in_year(person, year)
    return str(age) if age else "?"

def namify(x):
    return x.replace(' ', '_')

def make_name_with_email(name, email):
    return (T.a(href="email:"+email)[name]
            if email and email != ""
            else name)

def row(*things):
    return T.table(width="100%")[T.tr[[T.td[thing] for thing in things]]]

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

def weight_section():
    return T.img(src="weight-stone.png")

def spending_section():
    return T.div[T.p["Full details ", T.a(href="by-class.html")["here"], "."],
                 T.img(src="by-class.png")]

def timetable_section():
    return T.table[
        [[T.tr[T.td[slot.start.strftime("%H:%M")], T.td[slot.activity]]
          for slot in announce.get_day_announcer(os.path.expandvars("$COMMON/timetables/timetable.csv")).ordered()]]]

def budgetting_section(config, charts_dir):
    thresholds = classify.read_thresholds(config, "budgetting-thresholds.yaml")
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
        [T.tr[T.td(class_='birthday')[str(birthday(person, this_year))],
              T.td(class_='name')[make_name_with_email(contacts_data.make_name(person),
                                                       person.get('Primary email', ""))],
              T.td(class_='age')[age_string(person, this_year)]]
         for person in sorted([person
                              for person in people_by_id.values()
                              if birthday_soon(person, this_year, today)],
                              key=lambda person: birthday(person, this_year))]]

def contact_section():
    people_by_id, _ = contacts_data.read_contacts(os.path.expandvars("$COMMON/org/contacts.csv"))
    today = datetime.date.today()
    this_year = today.year
    return T.table(class_='contact_soon')[
        T.tr[T.th["Last contacted"], T.th["Name"]],
        [T.tr[T.td(class_='last_contacted')[str(last_contacted(person))],
              T.td(class_='name')[make_name_with_email(contacts_data.make_name(person),
                                                       person.get('Primary email', ""))]]
         for person in sorted([person
                              for person in people_by_id.values()
                              if contact_soon(person, today)],
                              key=lambda person: last_contacted(person))]]

def perishables_section():
    return T.table[[T.tr[T.td[row['Best before'].isoformat()],
                         T.td[row['Product']],
                         T.td[str(row['Quantity'])]]
        for row in perishables.get_perishables()]]

def diet_section():
    return T.div(class_="dietary")[
        T.h3["Calories by meal"],
        T.img(src="meal_calories.png"),
        T.h3("Food groups"),
        T.img(src="origin_calories.png")]

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
    page.add_section("Spending", spending_section())
    page.add_section("Monthly budgets", budgetting_section(config, charts_dir))
    page.add_section("Upcoming birthdays", birthdays_section())
    page.add_section("People to contact", contact_section())
    page.add_section("Food to use up in fridge", perishables_section())
    page.add_section("Diet", diet_section())
    # page.add_section("Exercise", T.p["placeholder for exercise data from MFP"])
    # page.add_section("Actions", T.p["placeholder for data from org-ql"])
    # page.add_section("Shopping", T.p["placeholder for data from org-ql"])
    # page.add_section("Travel", T.p["placeholder for google movement tracking"])
    page.add_section("Text for reflection", reflection_section())
    return [T.body[
        T.h1["My dashboard"],
        row(page.toc(), T.p[timetable_section()]),
        page.sections()]]

def page_text(page_contents, style_text, script_text):
    return untemplate.Serializer(untemplate.examples_vmap, 'utf-8').serialize(
        untemplate.HTML5Doc([untemplate.safe_unicode(style_text
                                                     + script_text),
                             page_contents]))

def write_dashboard_page(config, charts_dir):
    with open(os.path.join(charts_dir, "index.html"), 'w') as page_stream:
        page_stream.write(page_text(construct_dashboard_page(config, charts_dir),
                                    DASHBOARD_STYLESHEET, ""))

def file_newer_than_file(a, b):
    return os.path.getmtime(a) > os.path.getmtime(b)

def backup(filename, archive_dir, template):
    os.system("gzip --to-stdout %s > %s" % (
        filename,
        os.path.join(archive_dir,
                     (template % datetime.datetime.now().isoformat()) + ".gz")))

def latest_file_matching(template):
    files = glob.glob(template)
    print("looking for files matching", template, "and got", files)
    return files and sorted(files, key=os.path.getmtime)[-1]

def automatic_finances(config, charts_dir, begin_date, end_date, archive_dir, verbose):

    main_account = os.path.expandvars("$COMMON/finances/finances.csv")
    finances_completions = os.path.expandvars("$COMMON/var/finances-completions.el")
    bank_statement_template = os.path.expanduser("~/Downloads/Transaction*.csv")
    merge_results_dir = os.path.expanduser("~/scratch/auto-merge-results")
    merge_results_file = os.path.join(merge_results_dir, "merged-with-unmatched-all.csv")

    latest_bank_statement = latest_file_matching(bank_statement_template)

    if latest_bank_statement and file_newer_than_file(latest_bank_statement, main_account):
        print("Updating from latest bank statement", latest_bank_statement)
        if os.path.isdir(merge_results_dir):
            for file in os.listdir(merge_results_dir):
                os.remove(os.path.join(merge_results_dir, file))
        else:
            os.makedirs(merge_results_dir, exist_ok=True)
        finlisp.finlisp_main(["merge-latest-statement.lisp"],
                             merge_results_dir,
                             config,
                             verbose,
                             {'incoming-statement': latest_bank_statement})
        if os.path.isfile(merge_results_file):
            backup(main_account, archive_dir, "finances-to-%s.csv")
            shutil.copy(merge_results_file, main_account)
    else:
        print("Bank statement not newer than account file, so not updating")

    finlisp.finlisp_main(["chart-categories.lisp"],
                         charts_dir,
                         config,
                         verbose,
                         {'input-file': main_account,
                          'statements-file': os.path.expandvars("$COMMON/finances/handelsbanken/handelsbanken-full.csv"),
                          'classifiers-file': "budgetting-classes.yaml",
                          'thresholds-file': "budgetting-thresholds.yaml"})

    today = datetime.date.today()
    for begin, chart_filename in ([(begin_date, "by-class.png")]
                                  if begin_date
                                  else [(None, "by-class.png"),
                                        (back_from(today, None, None, 7), "by-class-past-week.png"),
                                        (back_from(today, None, 1, None), "by-class-past-month.png"),
                                        (back_from(today, None, 3, None), "by-class-past-quarter.png"),
                                        (back_from(today, 1, None, None), "by-class-past-year.png")]):
        qschart.qschart(os.path.join(charts_dir, "by-class.csv"),
                        'finances',
                        CATEGORIES_OF_INTEREST,
                        begin, end_date, None,
                        os.path.join(charts_dir, chart_filename))

    if file_newer_than_file(finances_completions, main_account):
        if verbose: print("updating finances completions")
        list_completions.list_completions()

def back_from(when, years_back, months_back, days_back):
    if months_back and months_back >= 12:
        years_back = (years_back or 0) + months_back / 12
        months_back %= 12
    if years_back:
        when = when.replace(year=when.year - years_back)
    if months_back:
        if months_back >= when.month:
            when = when.replace(year=when.year - 1, month=12 + when.month - months_back)
        else:
            when = when.replace(month=when.month - months_back)
    if days_back:
        when = when - datetime.timedelta(days=days_back)
    return datetime.datetime.combine(when, datetime.time())

def automatic_physical(charts_dir, begin_date, end_date, archive_dir):

    physical = os.path.expandvars("$COMMON/health/physical.csv")
    weight = os.path.expandvars("$COMMON/health/weight.csv")
    mfp_filename = os.path.expandvars("$COMMON/health/mfp-accum.csv")
    # TODO: temperature, blood pressure, peak flow
    phys_scratch = "/tmp/physical-tmp.csv"

    qsmerge.qsmerge(physical, [weight], None, phys_scratch)

    if check_merged_row_dates.check_merged_row_dates(phys_scratch, physical, weight):
        backup(physical, archive_dir, "physical-to-%s.csv")
        shutil.copy(phys_scratch, physical)
        today = datetime.date.today()
        for begin, template in ([(begin_date, "weight-%s.png")]
                                if begin_date
                                else [(None, "weight-%s.png"),
                                      (back_from(today, None, None, 7), "weight-past-week-%s.png"),
                                      (back_from(today, None, 1, None), "weight-past-month-%s.png"),
                                      (back_from(today, None, 3, None), "weight-past-quarter-%s.png"),
                                      (back_from(today, 1, None, None), "weight-past-year-%s.png")]):
            for units in ('stone', 'kilogram', 'pound'):
                qschart.qschart(physical,
                                'weight',
                                [units],
                                begin, end_date, None,
                                os.path.join(charts_dir, template % units))
    else:
        print("merge of physical data produced the wrong number of rows")

    qschart.qschart(mfp_filename,
                    'calories',
                    ['calories', 'breakfast', 'lunch', 'dinner', 'snacks'],
                    begin_date, end_date, None,
                    os.path.join(charts_dir, "meal_calories.png"))
    qschart.qschart(mfp_filename,
                    'food_groups',
                    ['carbohydrates', 'fat', 'protein', 'sugar'],
                    begin_date, end_date, None,
                    os.path.join(charts_dir, "origin_calories.png"))

def automatic_update_startpage():
    startpage = os.path.expanduser("~/public_html/startpage.html")
    startpage_source = os.path.expanduser("~/common/org/startpage.yaml")
    startpage_style = os.path.expanduser("~/common/org/startpage.css")
    if (os.path.getmtime(startpage_source) > os.path.getmtime(startpage)
        or os.path.getmtime(startpage_style) > os.path.getmtime(startpage)):
        os.system("make_link_table.py --output %s --stylesheet %s %s"
                  % (startpage, startpage_style, startpage_source))

def automatic_contacts(charts_dir, archive_dir):
    contacts_file = os.path.expandvars("$COMMON/org/contacts.csv")
    contacts_scratch = "/tmp/contacts_scratch.csv"
    link_contacts.link_contacts_main(contacts_file, False, False, contacts_scratch)
    with open(contacts_file) as confile:
        original_lines = len(confile.readlines())
    with open(contacts_scratch) as conscratch:
        scratch_lines = len(conscratch.readlines())
    if original_lines == scratch_lines:
        backup(contacts_file, archive_dir, "contacts-%s.csv")
        shutil.copy(contacts_scratch, contacts_file)
    else:
        print("wrong number of people after linking contacts, originally", original_lines, "but now", scratch_lines)

def make_tarball(tarball, parent_directory, of_directory):
    if not os.path.isfile(tarball):
        command = "tar cz -C %s %s > %s" % (parent_directory, of_directory, tarball)
        print("backup command is", command)
        os.system(command)

def automatic_backups():
    common_backups = os.path.expanduser("~/common-backups")
    daily_backup_template = "org-%s.tgz"
    weekly_backup_template = "common-%s.tgz"
    today = datetime.date.today()
    make_tarball(os.path.join(common_backups, daily_backup_template % today.isoformat()),
                 os.path.expandvars("$COMMON"),
                 "org")
    weekly_backup_day = 2    # Wednesday, probably the least likely day to be on holiday and not using the computer
    if today.weekday() == weekly_backup_day:
        make_tarball(os.path.join(common_backups, weekly_backup_template % today.isoformat()),
                     os.path.expandvars("$HOME"), "common")
    if today.day == 1:
        backup_isos_directory = os.path.expanduser("~/isos/backups")
        monthly_backup_name = os.path.join(backup_isos_directory, "backup-%s.iso" % today.isoformat())
        if not os.path.isfile(monthly_backup_name):
            # make_tarball("/tmp/music.tgz", os.path.expandvars("$HOME"), "Music")
            make_tarball("/tmp/github.tgz", os.path.expanduser("~/open-projects/github.com"), "hillwithsmallfields")
            files_to_backup = [
                latest_file_matching(os.path.join(common_backups, daily_backup_template % "*")),
                latest_file_matching(os.path.join(common_backups, weekly_backup_template % "*")),
                # too large for genisoimage:
                # "/tmp/music.tgz",
                "/tmp/github.tgz"]
            # look for the output of https://github.com/hillwithsmallfields/JCGS-scripts/blob/master/backup-confidential
            confidential_backup = latest_file_matching("/tmp/personal-*.tgz.gpg")
            if confidential_backup:
                files_to_backup.append(confidential_backup)
                digest = confidential_backup.replace('gpg', 'sha256sum')
                if os.path.isfile(digest):
                    files_to_backup.append(digest)
                sig = digest + ".sig"
                if os.path.isfile(sig):
                    files_to_backup.append(sig)
            print("Time to take a monthly backup of", files_to_backup, "into", monthly_backup_name)
            os.system("genisoimage -o %s %s" % (monthly_backup_name, " ".join(files_to_backup)))
            print("made backup in", monthly_backup_name)

def automatic_actions(charts_dir,
                      begin_date, end_date,
                      do_externals, verbose):
    archive_dir = os.path.expanduser("~/archive")
    configdir = os.path.expanduser("~/open-projects/github.com/hillwithsmallfields/qs/conf")
    conversions_dir = os.path.expandvars("$COMMON/finances")
    accounts_config = os.path.join(configdir, "accounts.yaml")
    conversions_config = os.path.join(conversions_dir, "conversions.yaml")

    config = qsutils.load_config(verbose, None, None, accounts_config, conversions_config)

    mfp_filename = os.path.expandvars("$COMMON/health/mfp-accum.csv")

    os.makedirs(charts_dir, exist_ok=True)

    automatic_finances(config, charts_dir, begin_date, end_date, archive_dir, verbose)
    automatic_physical(charts_dir, begin_date, end_date, archive_dir)
    automatic_contacts(charts_dir, archive_dir)
    if do_externals:
        if ((datetime.datetime.fromtimestamp(os.path.getmtime(mfp_filename))
             + datetime.timedelta(hours=23, minutes=30))
            < datetime.datetime.now()):
            print("Fetching data from myfitnesspal.com")
            mfp_reader.automatic(config, mfp_filename, verbose)
            print("Fetched data from myfitnesspal.com")
        else:
            print("myfitnesspal.com data fetched within the past day or so, so not doing again yet")
        # TODO: get oura.com data
        # TODO: get garmin data

    write_dashboard_page(config, charts_dir)
    automatic_update_startpage()
    automatic_backups()

def main():
    parser = qsutils.program_argparser()
    parser.add_argument("--charts", default=os.path.expanduser("~/public_html/dashboard"),
                        help="""Directory to write charts into.""")
    parser.add_argument("--begin",
                        help="""Earliest date to chart.""")
    parser.add_argument("--end",
                        help="""Latest date to chart.""")
    parser.add_argument("--no-externals", action='store_true',
                        help="""Don't pester external servers""")
    args = parser.parse_args()

    automatic_actions(args.charts,
                      args.begin,
                      args.end,
                      not args.no_externals,
                      args.verbose)

if __name__ == '__main__':
    main()
