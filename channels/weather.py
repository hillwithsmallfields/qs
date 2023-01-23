import csv
import datetime
import decouple
import json
import os
import pyowm
import sys

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

source_dir = os.path.dirname(os.path.realpath(__file__))

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

ensure_in_path(os.path.dirname(source_dir))

import qsutils.qsutils            # https://github.com/hillwithsmallfields/qs/blob/master/utils/qsutils.py
from channels.panels import switchable_panel
import dashboard.dashboard

ensure_in_path(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

ensure_in_path(os.path.join(my_projects, "noticeboard"))

import announce                 # https://github.com/hillwithsmallfields/noticeboard/blob/master/announce.py
import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

CATEGORIES_OF_INTEREST = ['Eating in', 'Eating out', 'Projects', 'Hobbies', 'Travel']

class Weather:

    def __init__(self, _begin_date, _end_date, verbose):

        """Fetch the short-term forecast from openweathermap, saving hourly extracts from it into a CSV file, and
        the sunrise and sunset data into a JSON file."""

        print("in Weather initializer")

        owm = pyowm.owm.OWM(decouple.config('OWM_API_KEY'))
        reg = owm.city_id_registry()
        city = lifehacking_config.file_config('weather', 'weather-city')
        country = lifehacking_config.file_config('weather', 'weather-country')
        loc_name = "%s,%s" % (city, country)
        list_of_locations = reg.locations_for(city, country)
        place = list_of_locations[0]
        weather_manager = owm.weather_manager()
        observation = weather_manager.weather_at_place(loc_name)
        with open(lifehacking_config.file_config('weather', 'sunlight-times-file'), 'w') as outstream:
            json.dump({'sunrise': datetime.datetime.fromtimestamp(observation.weather.sunrise_time()).time().isoformat(timespec='minutes'),
                       'sunset': datetime.datetime.fromtimestamp(observation.weather.sunset_time()).time().isoformat(timespec='minutes')},
                      outstream)
        weather = weather_manager.one_call(lat=place.lat, lon=place.lon,units='metric')
        forecast = [{
            'time': datetime.datetime.fromtimestamp(h.ref_time).isoformat()[:16],
            'status': h.detailed_status,
            'precipitation': h.precipitation_probability,
            'temperature': h.temp['temp'],
            'uvi': h.uvi,
            'wind-speed': h.wnd['speed'],
            'wind-direction': h.wnd['deg']
        } for h in weather.forecast_hourly]

        with open(lifehacking_config.file_config('weather', 'weather-filename'), 'w') as outstream:
            writer = csv.DictWriter(outstream, ['time', 'status', 'precipitation', 'temperature', 'uvi', 'wind-speed', 'wind-direction'])
            writer.writeheader()
            for hour in forecast:
                writer.writerow(hour)

        self.forecast = forecast
