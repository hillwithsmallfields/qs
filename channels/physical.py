from collections import defaultdict
import datetime
import os
import shutil
import sys

import channels.panels as panels
import dobishem
from expressionive.expressionive import htmltags as T
import expressionive.expridioms

def merge_garmin_downloads(downloads):
    """Merge the downloads.
    They will come in file modification order, and we want to keep
    only the latest if the files overlap.
    """
    activities_by_timestamp = dict()
    for download in downloads:
        for activity in download:
            activities_by_timestamp[activity['Date']] = activity
    return [
        activities_by_timestamp[when]
        for when in sorted(activities_by_timestamp.keys())
    ]

def convert_existing(raw):
    # TODO: convert duration strings to datetime.duration
    return raw

def convert_weight(raw):
    return {}

def convert_garmin(raw):
    # TODO: convert duration strings to datetime.duration
    return {}

def convert_isometric(raw):
    # TODO: convert duration strings to datetime.duration
    return {}

def combine_physical_data(incoming_lists):
    by_date = defaultdict(dict)
    for list_in in incoming_lists:
        for entry in list_in:
            existing = by_date[entry['Date']]
            match entry.get('Activity Type'):
                case 'Cycling':
                    for aspect in ['Cycling distance', 'Cycling elapsed time', 'Cycle moving time']:
                        if (got := existing.get(aspect)):
                            existing[aspect] += entry[aspect]
                        pass
	            'Cycling max speed'	'Cycling average speed'

                    pass
                case 'Running' | 'Trail Running'::
                    pass
                case 'Walking':
                    pass
                case 'Open Water Swimming':
                    pass
                case 'Other':
                    if entry.get('Title', "").startswith("Elliptical Trainer"):
                        pass
                case _:
                    pass
    # TODO: stringify everything for writing

class PhysicalPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.accumulated_garmin_downloads_filename = os.path.expandvars("$SYNCED/health/garmin-downloads.csv")
        self.combined_physical_data_filename = os.path.expandvars("$SYNCED/health/physical.csv")
        self.physical_data = None
        self.updated = None

    def name(self):
        return 'physical'

    def label(self):
        return 'Health'

    def fetch(self):
        """Fetch Garmin downloads and merge them into an accumulated file."""
        dobishem.storage.combined(
            self.accumulated_garmin_downloads_filename,
            merge_garmin_downloads,
            {filename: process_raw_garmin_file
             for filename in dobishem.in_modification_order("~/Downloads/Activities*.csv")})

    def update(self, **kwargs):

        """Merge incoming health-related data from various files, into one central file."""

        self.physical_data = dobishem.storage.combined(
            self.combined_physical_data_filename,
            combine_physical_data,
            {
                self.combined_physical_data_filename: convert_existing,
                os.path.expandvars("$SYNCED/health/weight.csv"): convert_weight,
                self.accumulated_garmin_downloads_filename: convert_garmin,
                os.path.expandvars("$SYNCED/health/isometric.csv"): convert_isometric,
            })

        self.updated = datetime.datetime.now()
        return self
