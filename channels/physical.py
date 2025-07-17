from collections import defaultdict
import datetime
import os
import shutil
import sys

import numpy as np
import pandas as pd

import qsutils
import channels.panels as panels
import dobishem
from dobishem.nested_messages import BeginAndEndMessages
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection, linked_image

# import physical.mfp_reader
# import physical.oura_reader

RE_READ_MEASUREMENT = False
RE_READ_EXERCISE = False

NO_TIME = datetime.timedelta(seconds=0)

ACTIVITIES = (('cycling', 'Cycle'), ('running', 'Run'), ('walking', 'Walk'), ('swimming', 'Swim'),)
ISOMETRICS = (('plank', "Plank"),)

def parse_duration(dur):
    return (datetime.timedelta(hours=int(dur[:2]),
                               minutes=int(dur[3:5]),
                               seconds=float(dur[6:]))
            if isinstance(dur, str)
            else (datetime.timedelta(seconds=dur)
                  if isinstance(dur, (int, float))
                  else NO_TIME))

def parse_date(when):
    return datetime.date.fromisoformat(when[:10])

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
    # 'Date',
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

def merge_incoming_csv(main_file_key, incoming_key,
                       begin_date, end_date,
                       match_key=None, match_value=None,
                       column_renames={},
                       transformations={}):

    """Merge a CSV of new (typically daily) readings into a long-term history, by date.  The incoming data may
    overlap with the data already in the file, in which case the new data takes precedence; it won't make
    duplicate rows.  Columns may be renamed between the two files, and the data may transformed, according
    to the argument dictionaries 'column_renames' and 'transformations'."""

    main_filename = facto.file_config('physical', main_file_key)
    incoming_filename = latest_file_matching(facto.file_config('physical', incoming_key))
    if incoming_filename:
        print("merging", incoming_filename, "into", main_filename, "with column_renames", column_renames, "and matches", match_key, match_value)
        qsutils.qsutils.trim_csv.trim_csv(incoming_filename) # Remove leading guff bytes added by financisto
        if (os.path.isfile(incoming_filename)
            and ((not os.path.isfile(main_filename))
                 or storage.file_newer_than_file(incoming_filename, main_filename))):
            data = {}
            with open(incoming_filename) as instream:
                for row in csv.reader(instream):
                    header = data.rename_columns(row, column_renames)
                    break           # just get the first row
            if os.path.isfile(main_filename):
                with open(main_filename) as instream:
                    data = {row['Date']: row
                            for row in csv.DictReader(instream)}
            original_length = len(data)
            with open(incoming_filename) as instream:
                additional = {row['Date']: row
                              for row in (data.transform_cells(data.rename_columns(raw, column_renames),
                                                               transformations)
                                          for raw in csv.DictReader(instream))
                              if data.matches(row, match_key, match_value)}
                data.update(additional)
            if len(data) > original_length:
                with open(main_filename, 'w') as outstream:
                    writer = csv.DictWriter(outstream, header)
                    writer.writeheader()
                    for date in sorted(data.keys()):
                        writer.writerow(data[date])
            return data
        else:
            print("Latest incoming file", incoming_filename, "older than main file", main_filename)
            return None
    else:
        print("could not find an incoming file matching", facto.file_config('physical', incoming_key), "for", incoming_key, "to merge into", main_filename)
        return None

def fetch_omron(facto, begin_date, end_date, verbose):

    """Extract data from a manually saved Omron file.

    Automatic fetcher or scraper possibly to come."""

    with BeginAndEndMessages("updating blood pressure data"):
        merge_incoming_csv('omron-filename', 'omron-incoming-pattern',
                           begin_date, end_date,
                           column_renames={'Measurement Date': 'Date',
                                           'Time Zone': 'Timezone',
                                           'SYS(mmHg)': 'SYS',
                                           'DIA(mmHg)': 'DIA',
                                           'Pulse(bpm)': 'Pulse',
                                           'Device': 'Device Model Name'},
                           transformations={'Irregular heartbeat detected': qsutils.qsutils.string_to_bool,
                                            'Body Movement': qsutils.qsutils.string_to_bool,
                                            'Date': qsutils.qsutils.normalize_date})

def fetch_oura(facto, begin_date, end_date, verbose):

    """Update my Oura records by fetching updates from Oura's cloud system."""

    oura_filename = facto.file_config('physical', 'oura-filename')
    data = {}
    physical.oura_reader.oura_read_existing(data, oura_filename)
    existing_rows = len(data)
    if begin_date is None:
        begin_date = qsutils.qsutils.earliest_unfetched(data)
    with BeginAndEndMessages("fetching data from oura", verbose=verbose):
        physical.oura_reader.oura_fetch(data, begin_date, end_date)
    if len(data) > existing_rows:
        physical.oura_reader.oura_write(data, oura_filename)
    elif len(data) < existing_rows:
        print("Warning: sleep data has shrunk on being fetched --- not writing it")
    return data

def fetch_mfp(facto, _begin_date, _end_date, verbose):

    """Fetch recent data from MyFitnessPal.com."""

    with BeginAndEndMessages("fetching data from myfitnesspal.com", verbose=verbose):
        return physical.mfp_reader.MFP(facto.file_config('physical', 'mfp-filename')).update(verbose)

def merge_garmin_downloads(downloads):
    """Merge the downloads.
    They will come in file modification order, and we want to keep
    only the latest if the files overlap.
    """
    activities_by_timestamp = dict()
    with BeginAndEndMessages("Merging Garmin downloads") as msgs:
        for download in downloads:
            for activity in download:
                if 'Date' not in activity:
                    msgs.print(f"Garmin activity with missing date: {activity}")
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

def convert_garmin(raw):
    return apply_conversions(raw, GARMIN_CONVERSIONS)

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
                if 'Date' in entry:
                    by_date[entry['Date']].update(entry)
                else:
                    print("Undated entry:", entry)
        return qsutils.qsutils.ensure_numeric_dates(
            [by_date[date] for date in sorted(by_date.keys())]
        )
        # TODO: stringify everything for writing

def identity(x):
    return x

def show_types(label, converted_df):
    prev = None
    print("comparing types for", label)
    for index, row in converted_df.iterrows():
        types = [type(elt) for elt in row.array]
        if types != prev:
            print(label, index, types)
        prev = types

class PhysicalPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
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

    def fetch(self, verbose=False, messager=None):
        """Fetch health-related downloads such as Garmin, and merge them into an accumulated file."""
        dobishem.storage.make(
            self.accumulated_garmin_downloads_filename,
            merge_garmin_downloads,
            {filename: lambda raw: raw
             for filename in dobishem.storage.in_modification_order("~/Downloads/Activities*.csv")},
            verbose=verbose, messager=messager)

    def files_to_write(self):
        return [self.accumulated_garmin_downloads_filename,
                self.combined_exercise_filename,
                self.combined_measurement_filename]

    def update(self, verbose=False, messager=None, **kwargs):

        """Merge incoming health-related data from various files, into two central files,
        one for exercise and one for measurements."""

        self.exercise_data = qsutils.qsutils.ensure_numeric_dates(
            dobishem.storage.make(
                self.combined_exercise_filename,
                combine_exercise_data,
                {
                    self.combined_exercise_filename: convert_exercise,
                    self.accumulated_garmin_downloads_filename: convert_garmin,
                    os.path.expandvars("$SYNCED/health/isometric.csv"): convert_isometric,
                },
                verbose=verbose, messager=messager))
        self.measurement_data = dobishem.storage.make(
                self.combined_measurement_filename,
                combine_measurement_data,
                {
                    self.combined_measurement_filename: convert_measurement,
                    os.path.expandvars("$SYNCED/health/weight.csv"): convert_weight,
                    # TODO: add blood pressure readings
                    # TODO: add thermometer readings
                    # TODO: add peak flow readings
                },
                verbose=verbose, messager=messager)
        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self,
                            date_suffix, begin_date, end_date,
                            chart_sizes, background_colour, foreground_colour,
                            verbose=False):
        """Prepare any images used by the output of the `html` method."""
        # TODO: rolling averages
        if self.measurement_dataframe is None:

            for row in self.measurement_data:
                for col in ['Stone', 'Lbs', 'Lbs total', 'Date number',
                            'St total', # 'Kg', 'Non-zero',
                            ]:
                    if col in row:
                        v = row[col]
                        if v:
                            row[col] = float(v)
                        else:
                            row[col] = 0.0
                    else:
                        row[col] = 0.0
            converted_df = pd.DataFrame.from_records(self.measurement_data)
            # converted_df.astype
            converted_df.replace("", np.nan, inplace=True)
            print("original columns", converted_df.columns)
            converted_df = converted_df[['Date', 'Stone', 'Lbs', 'Lbs total',
                                         # try to narrow down which column breaks it
                                         'St total', # 'Kg', 'Non-zero',
                                         ]]
            print("trimmed columns", converted_df.columns)
            converted_df['Date'] = pd.to_datetime(converted_df['Date'])

            self.measurement_dataframe = converted_df

        if self.exercise_dataframe is None:
            self.exercise_dataframe = (pd.read_csv(self.combined_exercise_filename)
                                       if RE_READ_EXERCISE # self.exercise_data is None
                                       else pd.DataFrame.from_records(self.exercise_data))
            self.exercise_dataframe['Date'] = pd.to_datetime(self.exercise_dataframe['Date'])
            print("exercise_dataframe\n", self.exercise_dataframe)
        with BeginAndEndMessages("plotting physical charts", verbose=verbose) as msgs:
            for units in ('stone', 'kilogram', 'pound'):
                qsutils.qschart.qscharts(
                    data=self.measurement_dataframe,
                    timestamp=None,
                    columns=[units],
                    foreground_colour=foreground_colour,
                    begin=begin_date, end=end_date, match=None,
                    by_day_of_week=False, # split_by_DoW
                    outfile_template=os.path.join(
                        self.charts_dir, "weight-%s-%s-%%s.png" % (units, date_suffix)),
                    plot_param_sets=chart_sizes,
                    vlines=None,
                    verbose=verbose,
                    messager=msgs)
            if False:
                qsutils.qschart.qscharts(
                    data=self.measurement_dataframe,
                    timestamp=None,
                    # columns=['systolic', 'diastolic', 'heart_rate'],
                    columns=['Resting pulse'],
                    foreground_colour=foreground_colour,
                    begin=begin_date, end=end_date, match=None,
                    by_day_of_week=False, # split_by_DoW
                    outfile_template=os.path.join(
                        self.charts_dir, "bp-%s-%%s.png" % (date_suffix)),
                    plot_param_sets=chart_sizes,
                    vlines=None,
                    verbose=verbose,
            messager=msgs)
            for activity, activity_label in ACTIVITIES:
                qsutils.qschart.qscharts(
                    data=self.exercise_dataframe,
                    timestamp=None,
                    columns=['%s %s' % (activity_label, factor)
                             for factor in (
                                     'distance',
                                     # 'elapsed time',
                                     # 'moving time',
                                     'max speed',
                                     'average speed')],
                    foreground_colour=foreground_colour,
                    begin=begin_date, end=end_date, match=None,
                    bar=True,
                    by_day_of_week=False, # split_by_DoW
                    outfile_template=os.path.join(
                        self.charts_dir, "%s-%s-%%s.png" % (activity, date_suffix)),
                    plot_param_sets=chart_sizes,
                    vlines=None,
                    verbose=verbose,
                    messager=msgs)

    def html(self):
        with BeginAndEndMessages("preparing physical HTML"):
            return T.div(class_="physical")[wrap_box(
                T.div(class_="measurements")[labelled_subsection(
                    "Measurements",
                    wrap_box(
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="weight-stone",
                            label="weight"),
                        linked_image(
                            charts_dir=self.charts_dir,
                            image_name="bp",
                            label="BP")),
                )],
                T.div(class_="exercise")[labelled_subsection(
                    "Exercise",
                    wrap_box(
                        [
                            linked_image(
                                charts_dir=self.charts_dir,
                                image_name=activity.lower(),
                                label=activity)
                            for activity in ("Cycling", "Running", "Walking", "Swimming")
                        ]
                    ))])]
