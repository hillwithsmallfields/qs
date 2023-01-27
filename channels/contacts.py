import os
import sys
import shutil

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

ensure_in_path(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
import backup

my_projects = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

import link_contacts # https://github.com/hillwithsmallfields/coimealta/blob/master/contacts/link_contacts.py

class Contacts:

    def __init__(self, facto, verbose):
        self.facto = facto
        self.verbose = verbose

    def update(self):

        """Preen my contacts file.  This checks for links between contacts, and does some analysis, which it returns
        as the result."""

        contacts_file = self.facto.file_config('contacts', 'contacts-file')
        contacts_scratch = "/tmp/contacts_scratch.csv"
        contacts_analysis = link_contacts.link_contacts_main(contacts_file, True, False, contacts_scratch)
        with open(contacts_file) as confile, open(contacts_scratch) as conscratch:
            original_lines = len(confile.readlines())
            scratch_lines = len(conscratch.readlines())
            if original_lines == scratch_lines:
                backup.backup(contacts_file, self.facto.file_config('backups', 'archive'), "contacts-%s.csv")
                shutil.copy(contacts_scratch, contacts_file)
            else:
                print("wrong number of people after linking contacts, originally", original_lines, "but now", scratch_lines)
        return contacts_analysis
