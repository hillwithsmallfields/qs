import csv
import datetime
import os
import sys

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

source_dir = os.path.dirname(os.path.realpath(__file__))

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

ensure_in_path(os.path.dirname(source_dir))

import qsutils.qsutils            # https://github.com/hillwithsmallfields/qs/blob/master/utils/qsutils.py
from channels.panels import switchable_panel
import dashboard.dashboard

ensure_in_path(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

ensure_in_path(os.path.join(my_projects, "noticeboard"))

import announce                 # https://github.com/hillwithsmallfields/noticeboard/blob/master/announce.py
import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

def CONF(*keys):
    return lifehacking_config.lookup(dashboard.dashboard.CONFIGURATION, *keys)

def FILECONF(*keys):
    return os.path.expanduser(os.path.expandvars(CONF(*keys)))

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

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
                  extra_files=[
                      day_full_file
                               for day_full_file in [
                                       os.path.join(FILECONF('timetables', 'timetables-dir'),
                                                    day_name)
                                       for day_name in (
                                               "%s.csv" % day_of_week,
                                               "%s-%s.csv" % (day_of_week,
                                                              'even'
                                                              if day.isocalendar().week % 2 == 0
                                                              else 'odd'))]
                      if os.path.isfile(day_full_file)]).ordered()]]]
    return T.form(action='log_done_timeslots')[
        table,
        T.input(type='submit',
                method='post',
                class_='activity_logger',
                value="Log activities")] if with_form else table
