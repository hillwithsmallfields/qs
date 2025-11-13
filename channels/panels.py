from abc import ABC, abstractmethod

import datetime
import os
import sys

import qsutils.qschart
from dobishem.nested_messages import BeginAndEndMessages

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

    def create_charts(self, data, columns, date_suffix, begin_date, end_date,
                     chart_sizes, foreground_colour, verbose=False, messager=None,
                     chart_type=None, **kwargs):
        """Helper method to create charts with common parameters.

        This reduces boilerplate by providing sensible defaults for the most
        common qscharts parameters while allowing customization via kwargs.

        Args:
            data: DataFrame or data to chart
            columns: List of column names to chart
            date_suffix: Time period suffix (e.g., 'past_month', 'all_time')
            begin_date: Start date for filtering
            end_date: End date for filtering
            chart_sizes: Dict of size configurations
            foreground_colour: Color for chart elements
            verbose: Whether to print verbose messages
            messager: Message handler for progress output
            chart_type: Optional description of chart type for logging
            **kwargs: Additional parameters to pass to qscharts()

        Common kwargs:
            - bar: Boolean for bar chart (default: False)
            - by_day_of_week: Boolean to split by day of week (default: False)
            - weight_units: For weight charts (e.g., 'stone', 'kilogram')
            - activity: For activity charts (e.g., 'cycling')
            - vlines: Vertical lines to draw
        """
        description = chart_type or "data"
        with BeginAndEndMessages(f"Charting {description}", verbose=verbose) as msgs:
            qsutils.qschart.qscharts(
                data=data,
                timestamp=None,
                columns=columns,
                foreground_colour=foreground_colour,
                begin=begin_date,
                end=end_date,
                matching=None,
                by_day_of_week=kwargs.get('by_day_of_week', False),
                chart_store=self.outputs,
                plot_param_sets=chart_sizes,
                vlines=kwargs.get('vlines', None),
                verbose=verbose,
                messager=messager or msgs,
                date_suffix=date_suffix,
                **{k: v for k, v in kwargs.items()
                   if k not in ('by_day_of_week', 'vlines')})

    def get_html(self, messager=None):
        """Return an expressionive HTML structure from the cached data."""
        if not self.saved_html:
            self.saved_html = self.html(messager)
        return self.saved_html

    @abstractmethod
    def html(self, messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return None
