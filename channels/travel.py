import datetime
import os

import pandas as pd

import qsutils
import channels.panels as panels
from dobishem.nested_messages import BeginAndEndMessages
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection, linked_image

class TravelPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.places = None
        self.travel = None
        self.refuelling = None
        self.updated = None

    def name(self):
        return "travel"

    def label(self):
        return "Travel"

    def fetch_travel(begin_date, end_date):
        # TODO: fetch from Google, updating facto.file_config('travel', 'travel-filename') and facto.file_config('travel', 'places-filename')

        with BeginAndEndMessages("fetching travel data"):
            return None

    def update(self, verbose=False, messager=None):
        # TODO: write travel section of QS code
        # travel_main(facto.file_config('travel', 'travel-filename'), facto.file_config('travel', 'places-filename'))
        # TODO: calculate distances
        self.places = pd.read_csv(os.path.expandvars("$SYNCED/travel/places/places.csv"))
        self.travel = pd.read_csv(os.path.expandvars("$SYNCED/travel/travel.csv"))
        self.travel['Date'] = pd.to_datetime(self.travel['Date'])
        self.refuelling = pd.read_csv(os.path.expandvars("$SYNCED/org/fuel.csv"))
        self.refuelling['Date'] = pd.to_datetime(self.refuelling['Date'])
        self.refuelling['Miles'] = self.refuelling['Mileage'].diff()
        self.refuelling['MilesPerLitre'] = self.refuelling['Miles'] / self.refuelling['Litres']
        self.refuelling['PoundsPerMile'] = self.refuelling['Cost'] / self.refuelling['Miles']
        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self, date_suffix, begin_date, end_date, chart_sizes, verbose=False):
        """Prepare any images used by the output of the `html` method."""
        with BeginAndEndMessages("Charting travel") as msgs:
            qsutils.qschart.qscharts(
                data=self.refuelling,
                timestamp=None,
                columns=['Miles', 'MilesPerLitre', 'PoundsPerMile'],
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

    def html(self):
        return T.div[
            wrap_box(
                labelled_subsection(
                    "Travel distances",
                    T.p["Distance chart will go here"]
                ),
                labelled_subsection(
                    "Fuel",
                    linked_image(
                        charts_dir=self.charts_dir,
                        image_name="fuel",
                        label="fuel")))]
