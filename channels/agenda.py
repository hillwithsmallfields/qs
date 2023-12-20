import datetime
import json
import os
import sys

from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section

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

    def update(self):

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

    def actions_section(self):
        return wrap_box([T.h3["Mending"],
                         org_ql_list(top_items(self.from_org["Mending"]))],
                        [T.h3["Physical making"],
                         org_ql_list(top_items(self.from_org["Physical making"]))],
                        [T.h3["Programming"],
                         org_ql_list(top_items(self.from_org["Programming"]))])

    def shopping_section(self):
        return wrap_box([T.h3["Supermarket"],
                         org_ql_list(self.from_org["Supermarket"])],
                        [T.h3["Online"],
                         org_ql_list(self.from_org["Online"])])

    def html(self):
        return wrap_box(
            labelled_section("Actions", self.actions_section()),
            labelled_section("Shopping", self.shopping_section()))
