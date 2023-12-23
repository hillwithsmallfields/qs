from collections import defaultdict
import datetime
import os
import shutil
import sys

import channels.panels as panels
import dobishem
from expressionive.expressionive import htmltags as T
import expressionive.expridioms

def parse_duration(dur):
    return datetime.timedelta(
        hours=int(dur[:2]),
        minutes=int(dur[3:5]),
        seconds=int(dur[6:]))

CELL_CONVERSIONS = {
    'Activity Type': str,
    'Avg Stroke Rate': float,
    'Avg. Swolf': float,
    'Calories': float,
    'Chest': float,
    'Comment': str,
    'Cycle average speed': float,
    'Cycle elapsed time': parse_duration,
    'Cycle max speed': float,
    'Cycle moving time': parse_duration,
    'Date number': int,
    'Date': lambda date_and_time: date.fromisoformat(date_and_time['Date'][:10]),
    'Diastolic (a.m.)': int,
    'Diastolic (p.m.)': int,
    'Distance': float,
    'Elapsed Time': parse_duration,
    'Hips': float,
    'Kg': float,
    'Lbs total': int,
    'Lbs': int,
    'Left calf': float,
    'Left forearm': float,
    'Left thigh': float,
    'Left upper arm': float,
    'Moving Time': parse_duration,
    'Peak flow (a.m.)': int,
    'Peak flow (p.m.)': int,
    'Percent lost in week': float,
    'Plank longest': int,
    'Plank total': int,
    'Resting pulse': int,
    'Right calf': float,
    'Right forearm': float,
    'Right thigh': float,
    'Right upper arm': float,
    'Run elapsed time': parse_duration,
    'Run moving time': parse_duration,
    'Stone': int,
    'Swim distance': float,
    'Swim time': parse_duration,
    'Systolic (a.m.)': int,
    'Systolic (p.m.)': int,
    'Time': parse_duration,
    'Total Strokes': int,
    'Waist': float,
}

def filter_conversions(*just_these):
    return {
        field: converter
        for field, converter in CELL_CONVERSIONS.items()
        if field in just_these
    }

def apply_conversions(raw, conversions):
    return {field: safe_call(converter, raw[field])
            for field, converter in conversions.items()
            if field in raw}

EXERCISE_CONVERSIONS = filter_conversions(
            'Date',
            'Date number',
            'Calories at gym',
            'Swim distance',
            'Swim time',
            'Plank longest',
            'Plank total',
            'Cycle distance',
            'Cycle elapsed time',
            'Cycle moving time',
            'Cycle max speed',
            'Cycle average speed',
            'Run distance',
            'Run elapsed time',
            'Run moving time',
            'Run average speed',
            'Comment')

MEASUREMENT_CONVERSIONS = filter_conversions(
    'Date',
    'Stone',
    'Lbs',
    'Date number',
    'Lbs total',
    'St total',
    'Kg',
    'Percent lost in week',
    'Resting pulse',
    'Systolic (a.m.)',
    'Diastolic (a.m.)',
    'Systolic (p.m.)',
    'Diastolic (p.m.)',
    'Peak flow (a.m.)',
    'Peak flow (p.m.)',
    'Waist',
    'Chest',
    'Hips',
    'Left upper arm',
    'Right upper arm',
    'Left forearm',
    'Right forearm',
    'Left thigh',
    'Right thigh',
    'Left calf',
    'Right calf',
    'Comment'    )

GARMIN_CONVERSIONS = filter_conversions(
            'Activity Type',
            'Avg Stroke Rate',
            'Avg. Swolf',
            'Calories',
            'Date',
            'Distance',
            'Elapsed Time',
            'Moving Time',
            'Swim distance',
            'Swim time',
            'Time',
            'Total Strokes')

def safe_call(fn, arg):
    try:
        return fn(arg)
    except:
        return None

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

def convert_exercise(raw):
    return apply_conversions(raw, EXERCISE_CONVERSIONS)

def convert_measurement(raw):
    return apply_conversions(raw, MEASUREMENT_CONVERSIONS)

def convert_weight(raw):
    st = int(raw['Stone'])
    lbs = int(row['Lbs'])
    total_lbs =  st * 14 + lbs
    return {
        'Date': datetime.date.fromisoformat(raw['Date']),
        'Stone': st,
        'Lbs', lbs,
        'Lbs total': total_lbs,
        'St total' total_lbs / 14,
        'Kg': total_lbs * 0.453592,
    }

def convert_garmin(raw):
    return apply_conversions(raw, GARMIN_CONVERSIONS)

def convert_isometric(raw):
    return {
        'Date': datetime.date.fromisoformat(raw['Date']),
        'Plank longest': int(raw['Plank longest mins']) * 60 + int(raw[',Plank longest secs']),
        'Plank total': int(raw['Plank totat mins']) * 60 + int(raw[',Plank totat secs']),
    }

def combine_exercise_data(incoming_lists):
    by_date = defaultdict(dict)
    for list_in in incoming_lists:
        for entry in list_in:
            existing = by_date[entry['Date']]
            match entry.get('Activity Type'):
                case 'Cycling':
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
        self.combined_exercise_filename = os.path.expandvars("$SYNCED/health/exercise.csv")
        self.exercise_data = None
        self.measurement_data = None
        self.updated = None

    def name(self):
        return 'physical'

    def label(self):
        return 'Health'

    def fetch(self):
        """Fetch health-related downloads such as Garmin, and merge them into an accumulated file."""
        dobishem.storage.combined(
            self.accumulated_garmin_downloads_filename,
            merge_garmin_downloads,
            {filename: process_raw_garmin_file
             for filename in dobishem.in_modification_order("~/Downloads/Activities*.csv")})

    def update(self, **kwargs):

        """Merge incoming health-related data from various files, into one central file."""

        self.exercise_data = dobishem.storage.combined(
            self.combined_exercise_filename,
            combine_exercise_data,
            {
                self.combined_exercise_filename: convert_exercise,
                self.accumulated_garmin_downloads_filename: convert_garmin,
                os.path.expandvars("$SYNCED/health/isometric.csv"): convert_isometric,
            })
        self.measurement_data = dobishem.storage.combined(
            self.combined_measurement_filename,
            combine_exercise_data,
            {
                self.combined_measurement_filename: convert_measurement,
                os.path.expandvars("$SYNCED/health/weight.csv"): convert_weight,
                # TODO: add blood pressure readings
                # TODO: add thermometer readings
                # TODO: add peak flow readings
            })

        self.updated = datetime.datetime.now()
        return self
