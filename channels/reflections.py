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
        self.reflection_count = 2
        self.reflections = []
        self.second = None

    def name(self):
        return "reflections"

    def label(self):
        return "Reflections"

    def update(self, verbose=False, messager=None, **kwargs):
        """Update the cached data."""
        self.reflections = []
        for i in range(self.reflection_count):
            new_reflection = random_reflection(reflections_dir)
            countdown = 4               # in case there's only one reflection available
            while new_reflection in self.reflections and countdown > 0:
                new_reflection = random_reflection(reflections_dir)
                countdown -= 1
            if new_reflection not in self.reflections:
                self.reflections.append(new_reflection)
        super().update(verbose, messager)
        return self

    def random_reflection(self):
        with open(self.storage.glob("*.txt", template='texts', texts="reflection")) as instream:
            return random.choice([line.strip() for line in instream if line != "\n"])

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div(class_='reflection')[
            [T.p(reflection)
             for reflection in self.reflections]]
