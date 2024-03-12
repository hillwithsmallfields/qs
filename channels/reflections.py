"""Example panel to copy and base new ones on."""

import datetime
import glob
import os
import random

import channels.panels as panels
from expressionive.expressionive import htmltags as T

class ReflectionsPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def name(self):
        return "reflections"

    def label(self):
        return "Reflections"

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data."""
        self.first = random_reflection(reflections_dir)
        self.second = random_reflection(reflections_dir)
        countdown = 4               # in case there's only one reflection available
        while self.second == self.first and countdown > 0:
            self.second = random_reflection(reflections_dir)
            countdown -= 1
        super().update(verbose, messager)
        return self

    def random_reflection(self):
        with open(self.storage.glob("*.txt", template='texts', texts="reflection")) as instream:
            return random.choice([line.strip() for line in instream if line != "\n"])

    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div(class_='reflection')[
            T.p[self.first],
            T.p[self.second]]
