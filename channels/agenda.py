import os
import sys

class Agenda:

    def __init__(self, facto, verbose):
        self.facto = facto
        self.verbose = verbose

    def update(self):

        """Also updates the parcels expected list.
        Files written:
        * $SYNCED/var/views.json
        * $SYNCED/var/parcels-expected.json"""

        os.system("emacs -q --script " +
                  os.path.expandvars("$MY_ELISP/special-setups/dashboard/dashboard-emacs-query.el"))

        # TODO: read something to return

        return None
