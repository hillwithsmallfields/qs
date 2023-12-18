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
        self.updated = datetime.datetime.now()
        return self

    def html(self):
        return wrap_box(
            labelled_section("Actions", actions_section(self.from_org)),
            labelled_section("Shopping", shopping_section(self.from_org)))
