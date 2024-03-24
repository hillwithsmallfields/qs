import datetime
import json
import os
import sys

from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section

import channels.panels as panels

class ParcelsPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.parcels = None

    def name(self):
        return 'parcels'

    def label(self):
        return 'Parcels expected'

    def reads_files(self, filenames):
        return "shopping.org" in filenames

    def fetch(self, verbose=False, messager=None):
        # The "fetch" operation for this is done by agenda.py
        pass

    def update(self, verbose=False, messager=None, **kwargs):
        self.parcels = self.storage.load(scratch="parcels-expected.json")
        messager.print("updated parcels to " + str(self.parcels))
        self.updated = datetime.datetime.now()
        super().update(verbose, messager)
        return self

    def html(self, messager=None):
        if self.parcels:
            dates = {}
            for parcel in self.parcels:
                messager.print("parsing %s" % parcel)
                date = datetime.date.fromisoformat(parcel[0])
                if date not in dates:
                    dates[date] = []
                dates[date].append(parcel[1])
            return [T.dl[[[T.dt[date.strftime("%Y-%m-%d %d")],
                           T.dd[T.ul[[[T.li[parcel]
                                       for parcel in sorted(dates[date])]]]]]]]]
        else:
            messager.print("Warning: no parcels data")
