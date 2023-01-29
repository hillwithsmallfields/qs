import datetime
import os
import sys

class Agenda:

    def __init__(self, facto):
        self.facto = facto
        self.updated = None

    def update(self, read_external, verbose):

        """Also updates the parcels expected list.
        Files written:
        * $SYNCED/var/views.json
        * $SYNCED/var/parcels-expected.json"""

        os.system("emacs -q --script " +
                  os.path.expandvars("$MY_ELISP/special-setups/dashboard/dashboard-emacs-query.el"))

        # TODO: read something to return

        self.updated = datetime.datetime.now()
        return self
