import csv
import datetime
import os
import sys

import channels.panels as panels

from expressionive.expressionive import htmltags as T
from expressionive.expridioms import switchable_panel
import dashboard.dashboard
import dobishem.dates

import timetable.announce as announce

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

class TimetablePanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.day_after_tomorrow_name = dobishem.dates.forward_from(datetime.date.today(), None, None, 2).strftime("%A")
        self.day_names = [
            'today',
            'tomorrow',
            self.day_after_tomorrow_name]
        self.days = [
            TimetableDay().html(with_form=True),
            TimetableDay(dobishem.dates.forward_from(datetime.date.today(), None, None, 1)).html(),
            TimetableDay(dobishem.dates.forward_from(datetime.date.today(), None, None, 2)).html(),
        ]

    def name(self):
        return 'timetable'

    def label(self):
        return 'Timetable'

    def update(self, verbose=False, messager=None):
        super().update(verbose, messager)
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
        with open(os.path.expandvars("$SYNCED/var/weather.csv")) as weatherstream:
            self.weather = {row['time']: row for row in csv.DictReader(weatherstream)}
        timetables_dir = os.path.expandvars("$SYNCED/timetables")
        self.slots = announce.get_day_announcer(
            os.path.join(timetables_dir, "timetable.csv"),
            [day_full_file
             for day_full_file in [
                     os.path.join(timetables_dir, day_name)
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
