import datetime
import json
import os
import sys

from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section

import channels.agenda as agenda

class ParcelsPanel(agenda.AgendaPanel):

    def update(self, verbose=False, messager=None, **kwargs):
        self.from_org = {
            "Ordered": agenda.load_agenda_file("$ORG/shopping.org", require_todo="ORDERED"),
            "Dispatched": agenda.load_agenda_file("$ORG/shopping.org", require_todo="DISPATCHED"),
        }
        super().update(verbose, messager, **kwargs)
        return self

    def html(self, messager=None):
        return wrap_box(
            labelled_subsection("Parcels",
                                self.agenda_subsections(["Ordered",
                                                         "Dispatched"], messager)))
