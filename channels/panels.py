from abc import ABC, abstractmethod

import os
import sys

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

source_dir = os.path.dirname(os.path.realpath(__file__))

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

class DashboardPanel(ABC):

    def __init__(self, charts_dir):
        self.charts_dir = charts_dir
        self.updated = None

    def name(self):
        return self.label().lower()

    @abstractmethod
    def label(self):
        return None

    def fetch(self, verbose=False, messager=None, **kwargs):
        """Fetch data from external sources."""
        pass

    def files_to_write(self):
        """Returns a list of files that the update methods is expected to write.
        Used to back up the old versions before an update."""
        return []

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data."""
        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self,
                            date_suffix, begin_date, end_date,
                            chart_sizes, background_colour, foreground_colour,
                            verbose=False):
        """Prepare any images used by the output of the `html` method."""
        pass

    @abstractmethod
    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return None
