"""Perishables panel to copy and base new ones on."""

import datetime

import channels.panels as panels
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, SectionalPage
import coimealta.inventory.perishables

class PerishablesPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def name(self):
        return "perishables"

    def label(self):
        return "Perishables"

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data."""
        self.perishables = coimealta.inventory.perishables.get_perishables()
        self.updated = datetime.datetime.now()
        return self

    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        today = self.updated.date()
        week_ahead = (self.updated + datetime.timedelta(days=7)).date()
        return (T.p["No items on record."]
                if len(self.perishables) == 0
                else T.table[
                        [T.tr[T.th(colspan="2")["Use by"],
                              T.th["Days left"],
                              T.th["Item"],
                              T.th["Quantity"]]],
                        [[T.tr(class_=("out_of_date"
                                       if row['Best before'] < today
                                       else ("use_soon"
                                             if row['Best before'] < week_ahead
                                             # TODO: convert near days to names
                                             else "use_later")))[
                                                     T.td[row['Best before'].isoformat()],
                                                     T.td[row['Best before'].strftime("%d")],
                                                     T.td(class_="days_left")[(row['Best before'] - today).days],
                                                     T.td[row['Product']],
                                                     T.td[str(row['Quantity'])]]
                          for row in self.perishables]]])
