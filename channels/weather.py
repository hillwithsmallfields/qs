import csv
import datetime
import decouple
import json
import os
import pyowm
import sys

import dobishem.dates
import channels.panels as panels

import qsutils.qsutils
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import switchable_panel
import dashboard.dashboard

import timetable.announce as announce

COMPASS_POINTS = ('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')

def compass_point_name(deg):
    return COMPASS_POINTS[int((int(deg) + (180 / len(COMPASS_POINTS))) // (360 / len(COMPASS_POINTS))) % len(COMPASS_POINTS)]

class WeatherPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.weather_table_file = os.path.expandvars("$SYNCED/var/weather.csv")
        self.sunlight_file = os.path.expandvars("$SYNCED/var/sunlight-times.json")
        self.forecast = None

    def name(self):
        return 'weather'

    def label(self):
        return 'Weather'

    def fetch(self, verbose=False, messager=None):

        """Fetch the short-term forecast from openweathermap,
        saving hourly extracts from it into a CSV file, and
        the sunrise and sunset data into a JSON file."""

        try:
            owm_key = decouple.config('OWM_API_KEY')
        except:
            print("No OWM_API_KEY.  Define it as an envvar.")
            return
        try:
            owm = pyowm.owm.OWM(owm_key)
        except:
            print("Could not log in to OWM")
            return
        reg = owm.city_id_registry()
        city = "Cambridge"
        country = "GB"
        loc_name = "%s,%s" % (city, country)
        list_of_locations = reg.locations_for(city, country)
        place = list_of_locations[0]
        weather_manager = owm.weather_manager()
        observation = weather_manager.weather_at_place(loc_name)
        if verbose:
            if messager:
                messager.print(f"weather observation is {observation}")
            else:
                print("weather observation is", observation)
        with open(self.sunlight_file, 'w') as outstream:
            json.dump({'sunrise': (datetime.datetime.fromtimestamp(observation.weather.sunrise_time())
                                   .time().isoformat(timespec='minutes')),
                       'sunset': (datetime.datetime.fromtimestamp(observation.weather.sunset_time())
                                  .time().isoformat(timespec='minutes'))},
                      outstream)
        weather = weather_manager.one_call(lat=place.lat, lon=place.lon,units='metric')
        self.forecast = [{
            'time': datetime.datetime.fromtimestamp(h.ref_time).isoformat()[:16],
            'status': h.detailed_status,
            'precipitation': h.precipitation_probability,
            'temperature': h.temp['temp'],
            'uvi': h.uvi,
            'wind-speed': h.wnd['speed'],
            'wind-direction': h.wnd['deg']
        } for h in weather.forecast_hourly]

        with open(self.weather_table_file, 'w') as outstream:
            writer = csv.DictWriter(outstream, ['time', 'status', 'precipitation', 'temperature', 'uvi', 'wind-speed', 'wind-direction'])
            writer.writeheader()
            for hour in self.forecast:
                writer.writerow(hour)

    def update(self, verbose=False, messager=None):
        if self.forecast is None:
            if os.path.exists(self.weather_table_file):
                with open(self.weather_table_file) as weatherstream:
                    self.forecast=list(csv.DictReader(weatherstream))
        super().update(verbose, messager)
        return self

    def one_day_weather_section(self, day=None):
        if self.forecast is None:
            return T.p["Could not read weather data."]
        # https://pyowm.readthedocs.io/en/latest/v3/code-recipes.html
        if day is None:
            day = datetime.date.today()
        day_of_week = day.strftime("%A")
        daystring = day.isoformat()
        return T.table(id_='weather')[
            T.caption["%s %s" % (day_of_week, daystring)],
            T.tr[T.th["Time"],
                 T.th["Temperature"],
                 T.th["Precipitation"],
                 T.th["Wind"],
                 T.th["Weather"]],
            [[T.tr(class_='inactive',
                   name=hour['time'][11:16])[
                       T.td(class_='weather weather_time')[hour['time'][11:19]],
                       T.td(class_='weather weather_temp')[str(round(float(hour['temperature']), 1))],
                       T.td(class_='weather weather_prec')[hour['precipitation']],
                       T.td(class_='weather weather_wind')[str(round(float(hour['wind-speed'])))
                            + " "
                            + compass_point_name(hour['wind-direction'])],
                       T.td(class_='weather weather_status')[hour['status']]]]
                        for hour in self.forecast
                            if hour['time'].startswith(daystring)]]

    def html(self):
        day_after_tomorrow = dobishem.dates.forward_from(datetime.date.today(), None, None, 2)
        day_after_tomorrow_name = day_after_tomorrow.strftime("%A")
        with open(self.sunlight_file) as sunlight_stream:
            sunlight_times = json.load(sunlight_stream)
        return T.div(class_='weather')[
            T.h2["Weather"],
            switchable_panel('weather_switcher',
                             {'today': self.one_day_weather_section(),
                              'tomorrow': self.one_day_weather_section(
                                  dobishem.dates.forward_from(datetime.date.today(),
                                                             None, None, 1)),
                              # day_after_tomorrow_name: one_day_weather_section(
                              #     day_after_tomorrow)
                             },
                             {'today': "Today",
                              'tomorrow': "Tomorrow",
                              # day_after_tomorrow_name: day_after_tomorrow_name
                             },
                             ['today', 'tomorrow',
                              # day_after_tomorrow_name
                             ],
                             'today'),
            T.h3["Daylight times"],
            T.dl[T.dt["Sunrise:"], T.dd[sunlight_times['sunrise']],
            T.dt["Sunset:"], T.dd[sunlight_times['sunset']]]]
