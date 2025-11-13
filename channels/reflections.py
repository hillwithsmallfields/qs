import datetime
import glob
import os
import random

import dobishem.storage
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
            new_reflection = self.random_reflection()
            countdown = 4               # in case there's only one reflection available
            while new_reflection in self.reflections and countdown > 0:
                new_reflection = self.random_reflection()
                countdown -= 1
            if new_reflection not in self.reflections:
                self.reflections.append(new_reflection)
        super().update(verbose, messager)
        return self

    def random_reflection(self):
        reflection_file = random.choice(self.storage.glob("*.txt", texts="reflection"))
        # dobishem.storage.load() for .txt files returns a list of lines
        lines = dobishem.storage.load(reflection_file)
        # Filter out empty lines and strip whitespace
        non_empty_lines = [line.strip() for line in lines if line.strip()]
        return random.choice(non_empty_lines) if non_empty_lines else ""

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div(class_='reflection')[
            [T.p(reflection)
             for reflection in self.reflections]]
