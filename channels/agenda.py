import datetime
import json
import os
import sys

from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection

import channels.panels as panels

def top_items(items):
    return sorted(items, key=lambda item: item['position-in-file'])[:12]

def org_ql_list(items):
    # TODO: make this scrollable
    return T.div(class_='agenda_list')[T.ul[[T.li[item['title']]
                                             for item in items]]]

class AgendaPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.updated = None
        self.from_org = None

    def name(self):
        return 'agenda'

    def label(self):
        return "Things to do"

    def update(self, verbose=False):

        """Also updates the parcels expected list.
        Files written:
        * $SYNCED/var/views.json
        * $SYNCED/var/parcels-expected.json"""

        os.system("emacs -q --script " +
                  os.path.expandvars("$MY_ELISP/special-setups/dashboard/dashboard-emacs-query.el"))

        with open(os.path.expandvars("$SYNCED/var/views.json")) as org_ql_stream:
            self.from_org = json.load(org_ql_stream)
        print("sections from org are", self.from_org.keys())
        self.updated = datetime.datetime.now()
        return self

    def agenda_subsections(self, keys):
        return wrap_box([labelled_subsection(key,
                                             org_ql_list(top_items(self.from_org[key])))
                        for key in keys])

    def html(self):
        return wrap_box(
            labelled_subsection("Actions",
                                self.agenda_subsections(["Mending",
                                                         "Physical making",
                                                         "Programming"])),
            labelled_subsection("Shopping",
                                self.agenda_subsections(["Supermarket",
                                                         "Mackays",
                                                         "Online"])))
