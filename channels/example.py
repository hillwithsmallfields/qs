"""Example panel to copy and base new ones on."""

import panels
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, SectionalPage

class ExamplePanel(panel.DashboardPanel):

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

    def fetch(self, **kwargs):
        """Fetch data from external sources."""
        pass

    def update(self, **kwargs):
        """Update the cached data."""
        self.examples = make_examples()

    def prepare_page_images(self, **kwargs):
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


    def name(self):
        return "example"
