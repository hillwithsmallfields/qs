import datetime
import os
import shutil
import sys

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

source_dir = os.path.dirname(os.path.realpath(__file__))

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

ensure_in_path(os.path.dirname(source_dir))

import backup

import qsutils.qsutils            # https://github.com/hillwithsmallfields/qs/blob/master/utils/qsutils.py
from channels.panels import switchable_panel

class Physical:

    def __init__(self, facto):
        self.facto = facto
        self.updated = None

    def update(self, read_externals, verbose):

        """Merge incoming health-related data from various files, into one central file."""

        physical = self.facto.file_config('physical', 'physical-filename')
        phys_scratch = "/tmp/physical-tmp.csv"

        physical_files = [self.facto.file_config('physical', 'weight-filename')
                         # TODO: merge the other physical files
                        ]

        by_date = qsutils.qsmerge.qsmerge(physical,
                                physical_files, None, phys_scratch)

        if qsutils.check_merged_row_dates.check_merged_row_dates(phys_scratch, physical, *physical_files):
            backup.backup(physical, self.facto.file_config('backups', 'archive'), "physical-to-%s.csv")
            shutil.copy(phys_scratch, physical)
        else:
            if verbose:
                print("merge of physical data produced the wrong number of rows")

        self.updated = datetime.datetime.now()
        return self
