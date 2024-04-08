"""Example panel to copy and base new ones on."""

import channels.panels as panels
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, SectionalPage

class EmptyPanel(panels.DashboardPanel):

    def __init__(self, store, outputs, *args, **kwargs):
        super().__init__(store, outputs, *args, **kwargs)

    def name(self):
        return "empty"

    def label(self):
        return "Empty panel"

    def files_to_write(self):
        """Returns a list of files that the update methods is expected to write.
        Used to back up the old versions before an update."""
        return ["$SYNCED/example/example-accum.csv"]

    def fetch(self, verbose=False, messager=None, **kwargs):
        """Fetch data from external sources."""
        pass

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data."""
        super().update(verbose, messager)
        return self

    def prepare_page_images(self,
                            date_suffix, begin_date, end_date,
                            chart_sizes, background_colour, foreground_colour,
                            verbose=False):
        """Prepare any images used by the output of the `html` method."""
        pass

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return T.p["This data is missing"]
