import datetime
import json
import os
import subprocess
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
        self.input_files = set(("general.org", "shopping.org", "projects.org", "Marmalade-work.org"))
        self.from_org = None

    def name(self):
        return 'agenda'

    def label(self):
        return "Things to do"

    def reads_files(self, filenames):
        return filenames & self.input_files

    def update(self, verbose=False, messager=None):

        """Also updates the parcels expected list.
        Files written:
        * $SYNCED/var/views.json
        * $SYNCED/var/parcels-expected.json"""
        messager.print("running emacs subprocess for agenda queries")
        result = subprocess.run(["emacs",
                                 "--no-init-file",
                                 "--batch",
                                 "--script",
                                 os.path.expandvars("$MY_ELISP/special-setups/dashboard/dashboard-emacs-query.el")],
                                encoding='utf8',
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                text=True)
        if verbose:
            if result.stdout:
                messager.print("emacs stdout:")
                for line in result.stdout.split("\n"):
                    messager.print("    "+ line)
            else:
                messager.print("Nothing on emacs stdout")
        if result.returncode == 0:
            with open(os.path.expandvars("$SYNCED/var/views.json")) as org_ql_stream:
                self.from_org = json.load(org_ql_stream)
            if verbose:
                messager.print(f"sections from org are {self.from_org.keys()}")
            self.updated = datetime.datetime.now()
        else:
            messager.print("Emacs run for org query failed")
        super().update(verbose, messager)
        return self

    def agenda_subsections(self, keys):
        return wrap_box(*[labelled_subsection(key,
                                              org_ql_list(section_list))
                          for key in keys
                          if len(section_list := top_items(self.from_org.get(key, []))) > 0])

    def html(self):
        return wrap_box(
            labelled_subsection("Actions",
                                self.agenda_subsections(["Today",
                                                         "Imminent",
                                                         "Weekend",
                                                         "Mending",
                                                         "Marmalade,"
                                                         "Physical making",
                                                         "Programming"])),
            labelled_subsection("Shopping",
                                self.agenda_subsections(["Supermarket",
                                                         "Mackays",
                                                         "Online"])))
