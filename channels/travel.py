from collections import defaultdict
import datetime
from math import acos, cos, sin, radians
import os

import pandas as pd

import qsutils
import channels.panels as panels
import dobishem.storage as storage
from dobishem.nested_messages import BeginAndEndMessages
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection, linked_image

class TravelPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.places_filename = "$SYNCED/travel/places/places.csv"
        self.travel_filename = "$SYNCED/travel/travel.csv"
        self.places = None
        self.travel = None
        self.refuelling = None
        self.distance_by_year_df = None
        self.by_years_chart_filename = os.path.join(self.charts_dir, "miles-by-year.png")
        self.updated = None

    def name(self):
        return "travel"

    def label(self):
        return "Travel"

    def files_to_write(self):
        """Returns a list of files that the update methods is expected to write.
        Used to back up the old versions before an update."""
        return [self.travel_filename]

    def fetch_travel(begin_date, end_date):
        # TODO: fetch from Google, updating facto.file_config('travel', 'travel-filename') and facto.file_config('travel', 'places-filename')

        with BeginAndEndMessages("fetching travel data"):
            return None

    def update(self, verbose=False, messager=None):
        # TODO: write travel section of QS code
        # travel_main(facto.file_config('travel', 'travel-filename'), facto.file_config('travel', 'places-filename'))
        self.places = storage.read_csv(self.places_filename,
                                       result_type=dict,
                                       key_column='Place')
        self.travel = storage.read_csv(self.travel_filename)
        for journey in self.travel:
            journey['Date'] = datetime.date.fromisoformat(journey['Date'])
        prev_place = None
        for destination in self.travel:
            place = self.places.get(destination['Destination'])
            if place:
                placename = place['Place']
                if placename in self.places:
                    lat2 = destination['Latitude'] = float(place['Latitude'])
                    lon2 = destination['Longitude'] = float(place['Longitude'])
                    if prev_place:
                        destination['Distance'] = int(acos((sin(radians(lat1)) * sin(radians(lat2)))
                                                           + (cos(radians(lat1)) * cos(radians(lat2)))
                                                           * (cos(radians(lon2) - radians(lon1)))) * 3959)
                    prev_place = place
                    lat1 = lat2
                    lon1 = lon2
        storage.write_csv(self.travel_filename, self.travel)
        self.distance_by_year = defaultdict(int)
        try:
            for journey in self.travel:
                self.distance_by_year[journey['Date'].year] += int(journey.get('Distance', 0) or 0)
            self.distance_by_year_df = pd.DataFrame([{'Year': int(year), 'Miles': miles} for year, miles in self.distance_by_year.items()])
        except Exception as problem:
            print("problem", problem, "in preparing distances image")
        self.refuelling = pd.read_csv(os.path.expandvars("$SYNCED/org/fuel.csv"))
        self.refuelling['Date'] = pd.to_datetime(self.refuelling['Date'])
        self.refuelling['Miles'] = self.refuelling['Mileage'].diff()
        self.refuelling['MilesPerLitre'] = self.refuelling['Miles'] / self.refuelling['Litres']
        self.refuelling['PoundsPerMile'] = self.refuelling['Cost'] / self.refuelling['Miles']
        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self,
                            date_suffix, begin_date, end_date,
                            chart_sizes, background_colour, foreground_colour,
                            verbose=False):
        """Prepare any images used by the output of the `html` method."""
        with BeginAndEndMessages("Charting travel") as msgs:
            qsutils.qschart.qscharts(
                data=self.refuelling,
                timestamp=None,
                columns=['Miles', 'MilesPerLitre', 'PoundsPerMile'],
                foreground_colour=foreground_colour,
                begin=begin_date, end=end_date, match=None,
                bar=False,
                by_day_of_week=False, # split_by_DoW
                outfile_template=os.path.join(
                    self.charts_dir, "fuel-%s-%%s.png" % date_suffix),
                plot_param_sets=chart_sizes,
                vlines=None,
                verbose=verbose,
                messager=msgs,
            )
            # TODO: make a chart of distance travelled per year
            if self.distance_by_year_df is not None:
                qsutils.qschart.barchart(self.distance_by_year_df,
                                         x_name='Year', y_name='Miles',
                                         filename=self.by_years_chart_filename,
                                         background_colour=background_colour,
                                         foreground_colour=foreground_colour)

    def html(self):
        return T.div[
            wrap_box(
                labelled_subsection(
                    "Travel distances",
                    T.img(src=self.by_years_chart_filename)
                ),
                labelled_subsection(
                    "Fuel",
                    linked_image(
                        charts_dir=self.charts_dir,
                        image_name="fuel",
                        label="fuel",
                        title="Fuel consumption")))]
