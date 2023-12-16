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

    def fetch(self, **kwargs):
        """Fetch data from external sources."""
        pass

    def update(self, **kwargs):
        """Update the cached data."""
        pass

    def prepare_page_images(self, **kwargs):
        """Prepare any images used by the output of the `html` method."""
        pass

    @abstractmethod
    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return None

    @abstractmethod
    def name(self):
        return None
