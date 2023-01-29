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

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

class Timetable:

    def __init__(self, facto):
        self.day_after_tomorrow_name = qsutils.qsutils.forward_from(datetime.date.today(), None, None, 2).strftime("%A")
        self.day_names = [
            'today',
            'tomorrow',
            self.day_after_tomorrow_name]
        self.days = [
            TimetableDay().html(with_form=True),
            TimetableDay(qsutils.qsutils.forward_from(datetime.date.today(), None, None, 1)).html(),
            TimetableDay(qsutils.qsutils.forward_from(datetime.date.today(), None, None, 2)).html(),
        ]
        pass

    def update(self, read_external, verbose):
        return self

    def html(self):
        return T.div(class_='timetable')[
            T.h2["Timetable"],
            switchable_panel(
                'timetable_switcher',
                {'today': self.days[0],
                 'tomorrow': self.days[1],
                 self.day_after_tomorrow_name: self.days[2]},
                {'today': "Today",
                 'tomorrow': "Tomorrow",
                 self.day_after_tomorrow_name: self.day_after_tomorrow_name},
                ['today',
                 'tomorrow',
                 self.day_after_tomorrow_name],
                'today')]

    def __repr__(self):
        return "<timetable %s>" % ", ".join(self.days)

class TimetableDay:

    def __init__(self, day=None):
        self.day = day or datetime.date.today()
        self.day_of_week = self.day.strftime("%A")
        with open(lifehacking_config.file_config('weather', 'weather-filename')) as weatherstream:
            self.weather = {row['time']: row for row in csv.DictReader(weatherstream)}
        self.slots = announce.get_day_announcer(
            os.path.join(lifehacking_config.file_config('timetables', 'timetables-dir'),
                         lifehacking_config.config('timetables', 'default-timetable')),
            [day_full_file
             for day_full_file in [
                     os.path.join(lifehacking_config.file_config('timetables', 'timetables-dir'),
                                  day_name)
                     for day_name in (
                             "%s.csv" % self.day_of_week,
                             "%s-%s.csv" % (self.day_of_week,
                                            'even'
                                            if self.day.isocalendar().week % 2 == 0
                                            else 'odd'))]
             if os.path.isfile(day_full_file)])

    def html(self, with_form=False):
        # TODO: possibly add columns for weather data for the same times
        # TODO: look up times in the weather
        # TODO: fetch from Google calendar (in update.py) and merge that in here
        table = T.table(id_="timetable")[
            T.caption["%s %s" % (self.day_of_week, self.day.isoformat())],
            [[T.tr(class_='inactive',
                   name=slot.start.strftime("%H:%M"))[
                       T.td(class_='time_of_day')[slot.start.strftime("%H:%M")],
                       T.td(class_='activity')[T.a(href=slot.link)[slot.activity]
                                               if slot.link
                                               else slot.activity],
                       (T.td[T.input(type='checkbox',
                                     name=slot.activity, id_=slot.activity,
                                     class_='activity_logger')[""]]) if with_form else []]
              for slot in self.slots.ordered()]]]
        return T.form(action='log_done_timeslots')[
            table,
            T.input(type='submit',
                    method='post',
                    class_='activity_logger',
                    value="Log activities")] if with_form else table

    def __repr__(self):
        return "<timetable day %s %s>" % (self.day, self.day_of_week)
