import datetime
import os

import orgparse

from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection

import channels.panels as panels

class AgendaItem:

    def __init__(self, heading, status, tags=None, properties=None, parent=None):
        self.heading = heading
        self.status = status
        self.tags = tags or {}
        self.properties = properties or {}
        self.parent = parent
        # caching:
        self._timestamp = None

    def __str__(self):
        return f"{self.status} {self.heading}"

    def last_state_change(self):
        if not self._timestamp:
            if 'last-state-change' in self.properties:
                iso = self.properties['last-state-change']
                self._timestamp = datetime.datetime.fromisoformat(iso[1:11] + "T" + iso[16:21])
        return self._timestamp

    def longname(self):
        return self.heading + (""
                               if self.parent is None
                               else (" (in " + self.parent + ")"))

def load_agenda_file(filename,
                     n_results=None,
                     require_todo=None,
                     require_tag=None):
    base_node = orgparse.load(os.path.expandvars(filename))
    todo_keys = base_node.env.todo_keys
    results = []

    def add_entry_conditionally(entry):
        if ((entry.todo == require_todo
             if require_todo
             else entry.todo in todo_keys)
            and (require_tag is None
                 or require_tag in entry.tags)):
            results.append(AgendaItem(entry.get_heading(),
                                      status=entry.todo,
                                      tags=entry.tags,
                                      properties=entry.properties,
                                      parent=(None
                                              if isinstance(entry.get_parent(), orgparse.node.OrgRootNode)
                                              else entry.get_parent().get_heading())))
    for top_entry in base_node.children:
        if top_entry.children:
            for sub_entry in top_entry.children:
                add_entry_conditionally(sub_entry)
                if n_results and len(results) == n_results:
                    break
        else:
            add_entry_conditionally(top_entry)
            if n_results and len(results) == n_results:
                break
    return results

class AgendaPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        print("in AgendaPanel.__init__")
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
        self.from_org = {
            "General": load_agenda_file("$ORG/general.org", n_results=12),
            "Weekend": load_agenda_file("$ORG/general.org", require_tag='weekend'),
            "Projects": load_agenda_file("$ORG/projects.org", n_results=12),
            "Marmalade": load_agenda_file("$SYNCED/vehicles/Marmalade/Marmalade-work.org", 6),
            "Mending": load_agenda_file("$ORG/general.org", require_tag='mending'),
            "Programming": load_agenda_file("$ORG/general.org", require_tag='programming'),
            "Makespace": load_agenda_file("$ORG/projects.org", n_results=6, require_tag="@Makespace"),
            "Ordered": load_agenda_file("$ORG/shopping.org", require_todo="ORDERED"),
            "Dispatched": load_agenda_file("$ORG/shopping.org", require_todo="DISPATCHED"),
            "Learning": load_agenda_file("$ORG/learning.org", require_todo="OPEN", n_results=6),
            "Supermarket": load_agenda_file("$ORG/shopping.org", require_tag='supermarket', require_todo="BUY"),
            "Mackays": load_agenda_file("$ORG/shopping.org", require_tag='Mackays', require_todo="BUY"),
            "Project parts": load_agenda_file("$ORG/shopping.org", require_tag='project_parts', require_todo="BUY"),
        }
        super().update(verbose, messager)
        return self

    def agenda_subsections(self, keys, messager=None):
        if self.from_org:
            things = [labelled_subsection(key, T.ul[[T.li[item.longname()] for item in section_list]])
                      for key in keys
                      if len(section_list := self.from_org.get(key, [])) > 0]
            return wrap_box(*things)
        else:
            messager.print("Warning: no agenda data")
            return T.p["No agenda data found"]

    def html(self, messager=None):
        return wrap_box(
            labelled_subsection("Actions",
                                self.agenda_subsections(["General",
                                                         "Projects",
                                                         "Weekend",
                                                         "Mending",
                                                         "Marmalade,"
                                                         "Physical making",
                                                         "Programming"], messager)),
            labelled_subsection("Shopping",
                                self.agenda_subsections(["Supermarket",
                                                         "Mackays"], messager)))
