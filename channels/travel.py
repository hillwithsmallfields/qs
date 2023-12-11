import datetime

import panels

class TravelPanel(panels.DashboardPanel):

    def __init__(self, facto):
        self.facto = facto
        self.updated = None

    def fetch_travel(begin_date, end_date):
        # TODO: fetch from Google, updating facto.file_config('travel', 'travel-filename') and facto.file_config('travel', 'places-filename')

        with BeginAndEndMessages("fetching travel data"):
            return None

    def update(self, read_external, verbose):
        # TODO: write travel section of QS code
        # travel_main(facto.file_config('travel', 'travel-filename'), facto.file_config('travel', 'places-filename'))
        # TODO: calculate distances
        self.updated = datetime.datetime.now()
        return self
