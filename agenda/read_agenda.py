#!/usr/bin/env python3

import orgparse
import os
import re

import dobishem.tabular_text

COLUMN_ORDER= [
    'status',
    'title',
    'scheduled',
    'group',
    'project',
    'tags',
    'created',
    'modified',
    'notes',
]

class Project:

    def __init__(self, node):
        self.title = node.get_heading()
        self.status = node.todo
        self.tags = node.tags
        self.description = node.get_body()

    def __str__(self):
        return f"<Project {self.title}>"

def load_projects():
    # TODO: handle subprojects?
    return {project.title: project
            for project in (Project(node)
                            for node in orgparse.load(os.path.expandvars("$ORG/projects.org")).children)}

class Action:

    def __init__(self, group, title, status, tags, created, modified, notes):
        self.group = group
        self.project = ""
        self.title = title
        self.status = status
        self.tags = tags
        self.created = created
        self.modified = modified
        self.notes = notes

    def __str__(self):
        return f"<Action {self.status} {self.title} ({self.group}) {self.tags}>"

    def tags_string(self):
        return " ".join(sorted(self.tags)) if self.tags else ""

    def as_dict(self):
        return {
            'status': self.status,
            'title': self.title,
            'tags': self.tags_string(),
            'group': self.group,
            'project': self.project,
            'created': self.created,
            'modified': self.modified,
            'notes': self.notes
        }

def org_to_iso(org_date):
    return (m.group(1) + "T" + m.group(2)
            if (m := re.match("\\[([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]) [MTWFS][uoehra][neduit] ([0-9][0-9]:[0-9][0-9])\\]", org_date))
            else "")

def node_to_action(group, node):
    return Action(group,
                  title=node.get_heading(),
                  status=node.todo,
                  tags=node.tags,
                  created=org_to_iso(node.get_property('first-seen', "")),
                  modified=org_to_iso(node.get_property('last-state-change', "")),
                  notes=node.get_body().replace("\n", "\\n"))

def text_to_tags(text):
    return set(text.split(':')) if text else set()

def dict_to_action(data):
    return Action(data.get('group', "general"),
                  title=data['title'],
                  status=data.get('status', "TODO"),
                  tags=text_to_tags(data.get('tags')),
                  created=data.get('created', ""),
                  modified=data.get('modified', ""),
                  notes=data.get('notes'))

def text_to_table(text):
    rows_source, _columns = dobishem.tabular_text.read_tabular_to_dicts(text.split("\n"))
    return [dict_to_action(row) for row in rows_source]

def load_agenda_file(file):
    """Read a old or new agenda file.
    The new ones contain the data in a table."""
    contents = orgparse.load(os.path.expandvars(file)).children
    return (text_to_table(contents[0].get_body())
            if contents[0].get_heading() == "Agenda"
            else [action
                  for action in (node_to_action(group.get_heading(), node)
                                 for group in contents
                                 for node in group.children)])

def load_agenda():
    return (load_agenda_file("$ORG/general.org")
            + load_agenda_file("$ORG/shopping.org"))

def save_agenda(agenda):
    """Write a new-style agenda file (with the data as a table)."""
    with open(os.path.expandvars("$HOME/scratch/agenda.org"), 'w') as outstream:
        outstream.write("* Agenda:\n")
        dobishem.tabular_text.write_tabular(
            outstream,
            [action.as_dict() for action in agenda],
            column_order=COLUMN_ORDER,
            margin="  ")

def main():
    projects = load_projects()
    for k, v in projects.items():
        print(k, v)
    agenda = load_agenda()
    save_agenda(agenda)

if __name__ == "__main__":
    main()
