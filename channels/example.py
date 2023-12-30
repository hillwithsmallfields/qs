"""Example panel to copy and base new ones on."""

import channels.panels as panels
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, SectionalPage

class ExamplePanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def name(self):
        return "example"

    def label(self):
        return "Example panel"

    def files_to_write(self):
        """Returns a list of files that the update methods is expected to write.
        Used to back up the old versions before an update."""
        return ["$SYNCED/example/example-accum.csv"]

    def fetch(self, verbose=False, messager=None, **kwargs):
        """Fetch data from external sources."""
        pass

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data."""
        self.examples = make_examples()
        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self,
                            date_suffix, begin_date, end_date,
                            chart_sizes, background_colour, foreground_colour,
                            verbose=False):
        """Prepare any images used by the output of the `html` method."""
        render_example_images(self.examples)

    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div[expressionive.expridioms.wrap_box(
            expressionive.expridioms.linked_image(
                charts_dir=self.dashboard_dir,
                image_name="Example images",
                label="Examples"),
            T.div[T.h3["Recent examples"],
                  recent_examples_table(self.examples, 14)],
            )]
