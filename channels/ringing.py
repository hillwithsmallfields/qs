import collections
import datetime

import dobishem.storage
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_subsection, linked_image, row

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

    def __init__(self, charts_dir):
        self.charts_dir = charts_dir
        self.dove = None
        self.towers = None
        self.visits = None
        self.by_bells = None
        self.by_weight = None
        self.by_year = None
        self.methods_rung = None
        self.by_stage = None
        self.updated = None

    def name(self):
        return "ringing"

    def label(self):
        return "Ringing"

    def fetch(self, verbose=False, **kwargs):
        if datetime.date.today().day == 1:
            towers.download_dove()

    def update(self, verbose=False, **kwargs):
        self.dove = towers.read_dove()
        self.visits = towers.read_visits()

        towers.towers_fill_in(self.dove, self.visits)
        towers.write_visits(self.visits)

        self.by_bells, self.by_weight, self.by_year = towers.classify_towers(self.visits)

        self.methods_rung = dobishem.storage.read_csv("$SYNCED/ringing/methods.csv")
        self.by_stage = collections.defaultdict(list)
        for method in self.methods_rung:
            self.by_stage[int(method['Stage'])].append(method['Method'])

        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self, **kwargs):
        """Prepare any images used by the output of the `html` method."""
        # TODO: Chart towers grabbed by year
        # TODO: Chart towers rung by weight
        pass

    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div(class_='ringing')[
            wrap_box(
                labelled_subsection(
                    "Towers visited",
                    T.table[T.tr[T.th["Bells"], T.th["Towers"]],
                            [T.tr[T.th()[str(k)],
                                  T.td()[str(self.by_bells[k])]]
                             for k in sorted(self.by_bells.keys())],
                            T.tr[T.th["Total"], T.td[str(len(self.visits))]]]),
                labelled_subsection(
                    "Methods rung",
                    T.table[T.tr[T.th["Stage"], T.th["Methods"]],
                            [T.tr[T.th[STAGE_NAMES[stage]],
                                  T.td[T.span(class_='overview')[str(len(self.by_stage[stage])),
                                                                 T.span(class_='details')[", ".join(sorted(self.by_stage[stage]))]
                                                                 ]]]
                             for stage in sorted(self.by_stage.keys())],
                            T.tr[T.th["Total"], T.td[str(len(self.methods_rung))]],]))]
