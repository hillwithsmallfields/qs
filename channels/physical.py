from collections import defaultdict
import datetime
import os
import shutil
import sys

import pandas as pd

import qsutils
import channels.panels as panels
import dobishem
from dobishem.nested_messages import BeginAndEndMessages
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, linked_image

NO_TIME = datetime.timedelta(seconds=0)
EXCEL_EPOCH = datetime.date(1899, 12, 31)

ACTIVITIES = (('cycling', 'Cycle'), ('running', 'Run'), ('walking', 'Walk'), ('swimming', 'Swim'))

def parse_duration(dur):
    return (datetime.timedelta(hours=int(dur[:2]),
                               minutes=int(dur[3:5]),
                               seconds=float(dur[6:]))
            if dur
            else NO_TIME)

def parse_date(when):
    return datetime.date.fromisoformat(when[:10])

def ensure_numeric_dates(table):
    for row in table:
        date = row['Date']
        row['Date number'] = ((datetime.date.fromisoformat(date) if isinstance(date, str) else date) - EXCEL_EPOCH).days
    return table

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
    'Date': parse_date,
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
    'Plank longest': parse_duration,
    'Plank total': parse_duration,
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
                if 'Date' not in activity:
                    print("Garmin activity with missing date:", activity)
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
    lbs = int(raw['Lbs'])
    total_lbs =  st * 14 + lbs
    return {
        'Date': datetime.date.fromisoformat(raw['Date']),
        'Stone': st,
        'Lbs': lbs,
        'Lbs total': total_lbs,
        'St total': total_lbs / 14,
        'Kg': total_lbs * 0.453592,
    }

def convert_garmin0(raw):
    return apply_conversions(raw, GARMIN_CONVERSIONS)

def convert_garmin(raw):
    result = convert_garmin0(raw)
    # print("garmin:", raw, "==>", result)
    return result

def convert_isometric(raw):
    return {
        'Date': datetime.date.fromisoformat(raw['Date']),
        'Plank longest': int(raw.get('Plank longest mins', 0) or 0) * 60 + int(raw.get('Plank longest secs', 0)) or 0,
        'Plank total': int(raw.get('Plank total mins', 0) or 0) * 60 + int(raw.get('Plank total secs', 0) or 0),
    }

def combine_exercise_data(incoming_lists):
    by_date = defaultdict(dict)
    with BeginAndEndMessages("Combining exercise data", verbose=False):
        for list_in in incoming_lists:
            for entry in list_in:
                entry_date = entry['Date']
                existing = by_date[entry_date]
                existing['Date'] = datetime.date.fromisoformat(entry_date) if isinstance(entry_date, str) else entry_date
                # existing['Avg HR'] # TODO: calculate this
                existing['Max HR'] = (max((existing.get('Max HR', 0) or 0), (entry.get('Max HR', 0) or 0))) or None
                existing['Calories'] = ((existing.get('Calories', 0) or 0) + (entry.get('Calories', 0) or 0)) or None
                for field in ['Calories at gym', 'Plank longest', 'Plank total', 'Comment']:
                    if field in entry and entry[field]:
                        existing[field] = entry[field]
                if entry.get('Distance'): # avoid malformed imports from MyFitnessPal
                    match entry.get('Activity Type'):
                        case 'Cycling':
                            existing['Cycle distance'] = existing.get('Cycle distance', 0) + entry.get('Distance', 0)
                            existing['Cycle moving time'] = (existing.get('Cycle moving time') or NO_TIME) + entry['Moving Time']
                            existing['Cycle elapsed time'] = (existing.get('Cycle elapsed time') or NO_TIME) + entry['Elapsed Time']
                        case 'Running' | 'Trail Running':
                            existing['Run distance'] = existing.get('Run distance', 0) + entry.get('Distance', 0)
                            existing['Run moving time'] = (existing.get('Run moving time') or NO_TIME) + entry.get('Moving Time')
                            existing['Run elapsed time'] = (existing.get('Run elapsed time') or NO_TIME) + entry.get('Elapsed Time')
                        case 'Walking':
                            existing['Walk distance'] = existing.get('Walk distance', 0) + entry.get('Distance', 0)
                            existing['Walk moving time'] = (existing.get('Walk moving time') or NO_TIME) + entry.get('Moving Time')
                            existing['Walk elapsed time'] = (existing.get('Walk elapsed time') or NO_TIME) + entry.get('Elapsed Time')
                        case 'Open Water Swimming':
                            existing['Swim distance'] = existing.get('Swim distance', 0) + entry['Distance']
                            existing['Swim time'] = (existing.get('Swim time') or NO_TIME) + entry.get('Moving Time', NO_TIME)
                            pass
                        # case 'Other':
                        #     if entry.get('Title', "").startswith("Elliptical Trainer"):
                        #         pass
                        case _:
                            pass
        return [by_date[date] for date in sorted(by_date.keys())]
        # TODO: stringify everything for writing?

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
        self.combined_measurement_filename = os.path.expandvars("$SYNCED/health/measurements.csv")
        self.exercise_data = None
        self.exercise_dataframe = None
        self.measurement_data = None
        self.measurement_dataframe = None
        self.updated = None

    def name(self):
        return 'physical'

    def label(self):
        return 'Health'

    def fetch(self, verbose=False):
        """Fetch health-related downloads such as Garmin, and merge them into an accumulated file."""
        dobishem.storage.combined(
            self.accumulated_garmin_downloads_filename,
            merge_garmin_downloads,
            {filename: lambda raw: raw
             for filename in dobishem.storage.in_modification_order("~/Downloads/Activities*.csv")})

    def update(self, verbose=False, **kwargs):

        """Merge incoming health-related data from various files, into two central files,
        one for exercise and one for measurements."""

        self.exercise_data = ensure_numeric_dates(
            dobishem.storage.combined(
                self.combined_exercise_filename,
                combine_exercise_data,
                {
                    self.combined_exercise_filename: convert_exercise,
                    self.accumulated_garmin_downloads_filename: convert_garmin,
                    os.path.expandvars("$SYNCED/health/isometric.csv"): convert_isometric,
                }))
        self.measurement_data = ensure_numeric_dates(
            dobishem.storage.combined(
                self.combined_measurement_filename,
                combine_measurement_data,
                {
                    self.combined_measurement_filename: convert_measurement,
                    os.path.expandvars("$SYNCED/health/weight.csv"): convert_weight,
                    # TODO: add blood pressure readings
                    # TODO: add thermometer readings
                    # TODO: add peak flow readings
            }))

        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self, date_suffix, begin_date, end_date, chart_sizes, verbose=False):
        """Prepare any images used by the output of the `html` method."""
        # TODO: rolling averages
        # TODO: convert existing data variables if not empty
        self.measurement_dataframe = pd.read_csv(self.combined_measurement_filename)
        self.measurement_dataframe['Date'] = pd.to_datetime(self.measurement_dataframe['Date'])
        self.exercise_dataframe = pd.read_csv(self.combined_exercise_filename)
        self.exercise_dataframe['Date'] = pd.to_datetime(self.exercise_dataframe['Date'])
        with BeginAndEndMessages("plotting physical charts", verbose=verbose):
            for units in ('stone', 'kilogram', 'pound'):
                qsutils.qschart.qscharts(
                    data=self.measurement_dataframe,
                    file_type='weight',
                    timestamp=None,
                    columns=[units],
                    begin=begin_date, end=end_date, match=None,
                    by_day_of_week=False, # split_by_DoW
                    outfile_template=os.path.join(
                        self.charts_dir, "weight-%s-%s-%%s.png" % (units, date_suffix)),
                    plot_param_sets=chart_sizes,
                    vlines=None,
                    verbose=verbose)
            qsutils.qschart.qscharts(
                data=self.measurement_dataframe,
                file_type='BP',
                timestamp=None,
                columns=['systolic', 'diastolic', 'heart_rate'],
                begin=begin_date, end=end_date, match=None,
                by_day_of_week=False, # split_by_DoW
                outfile_template=os.path.join(
                    self.charts_dir, "bp-%s-%%s.png" % (date_suffix)),
                plot_param_sets=chart_sizes,
                vlines=None,
                verbose=verbose)
            for activity, activity_label in ACTIVITIES:
                qsutils.qschart.qscharts(
                    data=self.exercise_dataframe,
                    file_type=activity_label,
                    timestamp=None,
                    columns=['%s %s' % (activity_label, factor)
                             for factor in (
                                     'distance',
                                     # 'elapsed time',
                                     # 'moving time',
                                     'max speed',
                                     'average speed')],
                    begin=begin_date, end=end_date, match=None,
                    bar=True,
                    by_day_of_week=False, # split_by_DoW
                    outfile_template=os.path.join(
                        self.charts_dir, "%s-%s-%%s.png" % (activity, date_suffix)),
                    plot_param_sets=chart_sizes,
                    vlines=None,
                    verbose=verbose)

    def html(self):
        with BeginAndEndMessages("preparing physical HTML"):
            return T.div(class_="physical")[wrap_box(
                T.div(class_="measurements")[
                    T.h3["Measurements"],
                    T.p["There are %d measurement rows." % len(self.measurement_data)],
                    wrap_box(
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="weight-stone",
                            label="weight"),
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="bp",
                            label="BP")),
                ],
                T.div(class_="exercise")[
                    T.h3["Exercise"],
                    T.p["There are %d exercise rows." % len(self.exercise_data)],
                    wrap_box(
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="cycling",
                            label="Cycling"),
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="running",
                            label="Running"),
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="walking",
                            label="Walking"),
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="swimming",
                            label="Swimming")),
                ])]
