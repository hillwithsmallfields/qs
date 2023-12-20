import datetime

import channels.panels as panels
from dobishem.nested_messages import BeginAndEndMessages
from expressionive.expressionive import htmltags as T
import expressionive.expridioms

class TravelPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.updated = None

    def name(self):
        return "travel"

    def label(self):
        return "Travel"

    def fetch_travel(begin_date, end_date):
        # TODO: fetch from Google, updating facto.file_config('travel', 'travel-filename') and facto.file_config('travel', 'places-filename')

        with BeginAndEndMessages("fetching travel data"):
            return None

    def update(self):
        # TODO: write travel section of QS code
        # travel_main(facto.file_config('travel', 'travel-filename'), facto.file_config('travel', 'places-filename'))
        # TODO: calculate distances
        self.updated = datetime.datetime.now()
        return self

    def html(self):
        return T.div[
            expressionive.expridioms.wrap_box(T.h2["Travel data to go here"])]
