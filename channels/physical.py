from collections import defaultdict
import datetime
import os
import shutil
import sys

import channels.panels as panels
import dobishem
from dobishem.nested_messages import BeginAndEndMessages
from expressionive.expressionive import htmltags as T
import expressionive.expridioms

def parse_duration(dur):
    return datetime.timedelta(
        hours=int(dur[:2]),
        minutes=int(dur[3:5]),
        seconds=int(dur[6:]))

CELL_CONVERSIONS = {
    'Activity Type': str,
    'Avg HR': float,
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
    'Max HR': float,
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
    'Run distance': float,
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
    'Walk distance': float,
    'Walk elapsed time': parse_duration,
    'Walk moving time': parse_duration,
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
    'Avg HR',
    'Calories at gym',
    'Cycle average speed',
    'Cycle distance',
    'Cycle elapsed time',
    'Cycle max speed',
    'Cycle moving time',
    'Date number',
    'Date',
    'Max HR',
    'Plank longest',
    'Plank total',
    'Run average speed',
    'Run distance',
    'Run elapsed time',
    'Run moving time',
    'Swim distance',
    'Swim time',
    'Walk average speed',
    'Walk distance',
    'Walk elapsed time',
    'Walk moving time',
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
    'Avg HR',
    'Avg Stroke Rate',
    'Avg. Swolf',
    'Calories',
    'Date',
    'Distance',
    'Elapsed Time',
    'Max HR',
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
    with BeginAndEndMessages("Merging Garmin downloads"):
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
    with BeginAndEndMessages("Combining exercise data"):
        for list_in in incoming_lists:
            for entry in list_in:
                existing = by_date[entry['Date']]
                # existing['Avg HR'] # TODO: calculate this
                existing['Max HR'] = max(existing.get('Max HR', 0), entry['Max HR'])
                existing['Calories'] = existing.get('Calories', 0) + entry['Calories']
                match entry.get('Activity Type'):
                    case 'Cycling':
                        existing['Cycle distance'] = existing.get('Cycle distance', 0) + entry['Distance']
                        existing['Cycle moving time'] = existing.get('Cycle moving time', 0) + entry['Moving Time']
                        existing['Cycle elapsed time'] = existing.get('Cycle elapsed time', 0) + entry['Elapsed Time']
                    case 'Running' | 'Trail Running'::
                        existing['Run distance'] = existing.get('Run distance', 0) + entry['Distance']
                        existing['Run moving time'] = existing.get('Run moving time', 0) + entry['Moving Time']
                        existing['Run elapsed time'] = existing.get('Run elapsed time', 0) + entry['Elapsed Time']
                    case 'Walking':
                        existing['Walk distance'] = existing.get('Walk distance', 0) + entry['Distance']
                        existing['Walk moving time'] = existing.get('Walk moving time', 0) + entry['Moving Time']
                        existing['Walk elapsed time'] = existing.get('Walk elapsed time', 0) + entry['Elapsed Time']
                    case 'Open Water Swimming':
                        existing['Swim distance'] = existing.get('Swim distance', 0) + entry['Distance']
                        existing['Swim time'] = existing.get('Swim time', 0) + entry['Moving Time']
                        pass
                    # case 'Other':
                    #     if entry.get('Title', "").startswith("Elliptical Trainer"):
                    #         pass
                    case _:
                        pass
        return [by_date[date] for date in sorted(by_date.keys())]
        # TODO: stringify everything for writing

def combine_measurement_data(incoming_lists):
    by_date = defaultdict(dict)
    with BeginAndEndMessages("Combining measurement data"):
        for list_in in incoming_lists:
            for entry in list_in:
                by_date[entry['Date']].update(entry)
        return [by_date[date] for date in sorted(by_date.keys())]
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

        """Merge incoming health-related data from various files, into two central files,
        one for exercise and one for measurements."""

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
            combine_measurement_data,
            {
                self.combined_measurement_filename: convert_measurement,
                os.path.expandvars("$SYNCED/health/weight.csv"): convert_weight,
                # TODO: add blood pressure readings
                # TODO: add thermometer readings
                # TODO: add peak flow readings
            })

        self.updated = datetime.datetime.now()
        return self

    def html(self):
        return T.div(class_="physical")[expressionive.expridioms.wrap_box(
            T.div(class_="measurements")[
                T.p["There are %d measurement rows." % len(self.measurement_data)]
            ],
            T.div(class_="exercise")[
                T.p["There are %d exercise rows." % len(self.exercise_data)]
            ]
