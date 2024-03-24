from abc import ABC, abstractmethod

import datetime
import os
import sys

class DashboardPanel(ABC):

    def __init__(self, store, outputs):
        self.storage = store
        self.outputs = outputs
        self.updated = None
        self.saved_html = None

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

    def reads_files(self, filenames):
        """Returns whether this class reads any of the given filenames."""
        return True

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data.
        Call this from subclasses (using super) as it will
        invalidate the cached HTML."""
        self.updated = datetime.datetime.now()
        self.saved_html = None
        return self

    def prepare_page_images(self,
                            date_suffix, begin_date, end_date,
                            chart_sizes, background_colour, foreground_colour,
                            verbose=False):
        """Prepare any images used by the output of the `html` method."""
        pass

    def get_html(self, messager=None):
        """Return an expressionive HTML structure from the cached data."""
        if not self.saved_html:
            self.saved_html = self.html(messager)
        return self.saved_html

    @abstractmethod
    def html(self, messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return None
