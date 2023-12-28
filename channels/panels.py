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

    def fetch(self, verbose=False, **kwargs):
        """Fetch data from external sources."""
        pass

    def update(self, verbose=False, **kwargs):
        """Update the cached data."""
        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self, **kwargs):
        """Prepare any images used by the output of the `html` method."""
        pass

    @abstractmethod
    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return None
