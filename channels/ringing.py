import collections
import datetime
import os

import pandas as pd
import matplotlib.pyplot as plt

import dobishem.storage
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection, linked_image, row

import qsutils
import channels.panels as panels
import ringing.tower_visits as towers

STAGE_NAMES=[
    "",
    "on 1",
    "on 2",
    "Singles",
    "Minimus",
    "Doubles",
    "Minor",
    "Triples",
    "Major",
    "Caters",
    "Royal",
    "Cinques",
    "Maximus",
    ]

class RingingPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.by_years_chart_filename = self.outputs.resolve(chart="towers-by-year")
        self.input_files = set(("towers.csv", "methods.csv"))
        self.dove = None
        self.towers = None
        self.visits = None
        self.by_bells = None
        self.by_weight = None
        self.by_year = None
        self.by_year_df = None
        self.methods_rung = None
        self.by_stage = None

    def name(self):
        return "ringing"

    def label(self):
        return "Ringing"

    def reads_files(self, filenames):
        return filenames & self.input_files

    def fetch(self, verbose=False, messager=None, **kwargs):
        if datetime.date.today().day == 1:
            if verbose:
                if messager:
                    messager.print("Downloading Dove data")
                else:
                    print("Downloading Dove data")
            towers.download_dove()

    def files_to_write(self):
        return [os.path.expandvars("$SYNCED/ringing/towers.csv")]

    def update(self, verbose=False, messager=None, **kwargs):
        self.dove = towers.read_dove()
        self.visits = towers.read_visits()

        towers.towers_fill_in(self.dove, self.visits)
        towers.write_visits(self.visits)

        self.by_bells, self.by_weight, self.by_year = towers.classify_towers(self.visits)

        self.methods_rung = dobishem.storage.read_csv("$SYNCED/ringing/methods.csv")
        self.by_stage = collections.defaultdict(list)
        for method in self.methods_rung:
            self.by_stage[int(method['Stage'])].append(method['Method'])
        ringing_years = list(self.by_year.keys())
        year_data = [{'Date': y, 'Towers': self.by_year.get(y, 0)}
                     for y in range(min(ringing_years), max(ringing_years)+1)]
        self.by_year_df = pd.DataFrame(year_data)
        super().update(verbose, messager)
        return self

    def prepare_page_images(self,
                            date_suffix, begin_date, end_date,
                            chart_sizes, background_colour, foreground_colour,
                            verbose=False):
        """Prepare any images used by the output of the `html` method."""
        # TODO: Chart towers grabbed by year
        # TODO: Chart towers rung by weight
        if self.by_year_df is not None:
            qsutils.qschart.barchart(self.by_year_df,
                                     x_name='Date', y_name='Towers',
                                     filename=self.by_years_chart_filename,
                                     background_colour=background_colour,
                                     foreground_colour=foreground_colour)

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div(class_='ringing')[
            wrap_box(
                labelled_subsection(
                    "Towers visited",
                    wrap_box([T.table[T.tr[T.th["Bells"], T.th["Towers"]],
                                      [T.tr[T.th()[str(k)],
                                            T.td()[str(self.by_bells[k])]]
                                   for k in sorted(self.by_bells.keys())],
                                      T.tr[T.th["Total"], T.td[str(len(self.visits))]]],
                              T.img(src=self.by_years_chart_filename)
                              ])
                ),
                labelled_subsection(
                    "Methods rung",
                    T.table[T.tr[T.th["Stage"], T.th["Methods"]],
                            [T.tr[T.th[STAGE_NAMES[stage]],
                                  T.td[T.span(class_='overview')[str(len(self.by_stage[stage])),
                                                                 T.span(class_='details')[", ".join(sorted(self.by_stage[stage]))]
                                                                 ]]]
                             for stage in sorted(self.by_stage.keys())],
                            T.tr[T.th["Total"], T.td[str(len(self.methods_rung))]],]))]
