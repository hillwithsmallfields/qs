import datetime
import json
import os
import sys

from expressionive.expridioms import wrap_box, labelled_section, SectionalPage

import channels.panels as panels

class AgendaPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.updated = None
        self.parcels = None

    def name(self):
        return 'parcels'

    def update(self, read_external, verbose):
        with open(os.path.expandvars("$SYNCED/var/parcels-expected.json")) as parcels_stream:
            self.parcels = json.load(parcels_stream)['expected']
        self.updated = datetime.datetime.now()
        return self

    def html(self):
        dates = {}
        for parcel in self.parcels:
            date = datetime.date.fromisoformat(parcel[0])
            if date not in dates:
                dates[date] = []
            dates[date].append(parcel[1])
        return [T.dl[[[T.dt[date.isoformat() + " " + DAYNAMES[date.weekday()]],
                       T.dd[T.ul[[[T.li[parcel]
                                   for parcel in sorted(dates[date])]]]]]
                      for date in sorted(dates)]]]