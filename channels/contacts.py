import datetime
import os
import sys
import shutil

import channels.panels as panels

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

ensure_in_path(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import backup

import coimealta.contacts.link_contacts as contacts
import coimealta.contacts.contacts_data as contacts_data

from expressionive.expressionive import htmltags as T

def make_name_with_email(name, email):
    return (T.a(href="email:"+email)[name]
            if email and email != ""
            else name)

def counts_table(caption, group):
    pairs = [(name, len(members)) for name, members in group.items()]
    s = sorted(pairs, key=lambda p: p[1])
    r = reversed(s)
    return T.div(class_='contacts_characteristics')[T.table[
        T.caption[caption],
        [T.tr[T.td[name], T.td[str(members)]]
         for name, members in r]]]

class ContactsPanel(panels.DashboardPanel):

    def __init__(self, facto):
        self.facto = facto
        self.contacts_analysis = None
        self.people_by_id = None
        self.people_by_name = None
        self.updated = None

    def name(self):
        return 'contacts'

    def update(self, read_external, verbose):

        """Preen my contacts file.  This checks for links between contacts, and does some analysis, which it returns
        as the result."""

        contacts_file = self.facto.file_config('contacts', 'contacts-file')
        contacts_scratch = "/tmp/contacts_scratch.csv"
        self.people_by_id, self.people_by_name = contacts_data.read_contacts(self.facto.file_config('contacts', 'contacts-file'))
        self.contacts_analysis = link_contacts.analyze_contacts(self.people_by_id)
        link_contacts.link_contacts(self.people_by_id, self.people_by_name)
        contacts_data.write_contacts(contacts_scratch, self.people_by_name)
        with open(contacts_file) as confile, open(contacts_scratch) as conscratch:
            original_lines = len(confile.readlines())
            scratch_lines = len(conscratch.readlines())
            if original_lines == scratch_lines:
                backup.backup(contacts_file, self.facto.file_config('backups', 'archive'), "contacts-%s.csv")
                shutil.copy(contacts_scratch, contacts_file)
            else:
                print("wrong number of people after linking contacts, originally", original_lines, "but now", scratch_lines)

        self.updated = datetime.datetime.now()
        return self

    def html(self):
        if self.contacts_analysis is None:
            return None
        n_people = self.contacts_analysis['n_people']
        today = datetime.date.today()
        this_year = today.year
        long_uncontacted = [person
                            for person in self.people_by_id.values()
                            if contacts_data.contact_soon(person, today, days_since_last_contact=90)]
        return panels.wrap_box(
            panels.labelled_section("Birthdays",
                                    T.table(class_='birthdays')[
                                        T.tr[T.th["Birthday"], T.th["Name"], T.th["Age"]],
                                        [T.tr[T.td(class_='birthday')[str(contacts_data.birthday(person, this_year))],
                                              T.td(class_='name')[make_name_with_email(contacts_data.make_name(person),
                                                                                       person.get('Primary email', ""))],
                                              T.td(class_='age')[contacts_data.age_string(person, this_year)]]
                                         for person in sorted([person
                                                               for person in self.people_by_id.values()
                                                               if contacts_data.birthday_soon(person, this_year, today, within_days=31)],
                                                              key=lambda person: contacts_data.birthday(person, this_year))]]),
            panels.labelled_section("To contact",
                                    T.table(class_='contact_soon')[
                                        T.tr[T.th["Last contacted"], T.th["Name"]],
                                        [T.tr[T.td(class_='last_contacted')[str(contacts_data.last_contacted(person))],
                                              T.td(class_='name')[make_name_with_email(contacts_data.make_name(person),
                                                                                       person.get('Primary email', ""))]]
                                  for person in sorted(long_uncontacted,
                                                       key=lambda person: contacts_data.last_contacted(person))]]),
            panels.labelled_section("People in contacts file",
                                    T.dl[
                                        T.dt["Number of people"], T.dd[str(n_people)],
                                        T.dt["By gender"],
                                        T.dd["; ".join(["%s: %d" % (k, len(v))
                                                        for k, v in self.contacts_analysis['by_gender'].items()])],
                                        T.dt["Ordained"],
                                        T.dd["%d (%d%% of total)" % (self.contacts_analysis['ordained'],
                                                                     round(100*self.contacts_analysis['ordained']/n_people))],
                                        T.dt["Dr/Prof"],
                                        T.dd["%d (%d%% of total)" % (self.contacts_analysis['doctored'],
                                                                     round(100*self.contacts_analysis['doctored']/n_people))],
                                        T.dt["flagged"],
                                        T.dd[T.dl[[[T.dt[flag], T.dd[[str(len(people))]]]
                                                   for flag, people in self.contacts_analysis['flagged'].items()
                                                   ]]]
                                    ]),
            panels.labelled_section("People groups",
                                    panels.row(counts_table("By nationality",
                                                            self.contacts_analysis['by_nationality']),
                                               counts_table("By title",
                                                            self.contacts_analysis['by_title']),
                                               counts_table("By place met",
                                                            self.contacts_analysis['by_place_met']))))
