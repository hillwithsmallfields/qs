#!/usr/bin/env python3

import os.path
import re
import shutil
import subprocess
import traceback

import channels.panels as panels
from expressionive.expressionive import htmltags as T
import expressionive.exprpages as exprpages

from motion_monitor import motion_monitor, managed_directory

SNAPS_DIRECTORY = "/tmp/snaps"

def local_copy(original, directory):
    """Take a copy of a file into a directory, and return its name within the directory."""
    short = os.path.split(original)[1]
    os.makedirs(directory, exist_ok=True)
    shutil.copy(original, os.path.join(directory, short))
    return short

def add_key(filename, details):
    """Add a key field to a file details dictionary."""
    m = re.search(r"(cam-[0-9]+-event-[0-9]+)", filename)
    if m:
        details['key'] = m.group(1)
        
class MotionPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        self.motion_clips_directory = None
        self.videos = None
        self.snaps = None
        try:
            self.motion_clips_directory = motion_monitor.get_clips_directory()
            print("motion clips directory is", self.motion_clips_directory)
        except RuntimeError:
            print("This does not seem to be a motion host.")
        super().__init__(*args, **kwargs)

    def name(self):
        return "motion"

    def label(self):
        return "Motion panel"

    def files_to_write(self):
        """Returns a list of files that the update methods is expected to write.
        Used to back up the old versions before an update."""
        return ["$SYNCED/motion/motion-summary.csv"]

    def fetch(self, verbose=False, messager=None, **kwargs):
        """Fetch data from external sources."""

        messager.print("in MotionPanel.fetch with clips directory %s" % self.motion_clips_directory)
        if self.motion_clips_directory:
            try:
                self.videos = {v['key']: v
                               for v in managed_directory.recent_files_in_directory(self.motion_clips_directory,
                                                                                    matching=lambda f: f.endswith(".mkv"),
                                                                                    augment=add_key)}
                self.snaps = managed_directory.recent_files_in_directory(self.motion_clips_directory,
                                                                         matching=lambda f: f.endswith(".jpg"),
                                                                         augment=add_key)
                for snap in self.snaps:
                    snap['video'] = self.videos.get(snap['key'])
            except Exception as e:
                messager.print("Got exception %s while getting recent motion clips and snaps" % e)
        else:
            messager.print("No motion clips directory")

    def update(self, verbose=False, messager=None, private_charts=None, **kwargs):
        """Update the cached data."""
        messager.print("in MotionPanel.update with private_charts %s and snap files %s" % (private_charts, self.snaps))
        if self.snaps and private_charts:
            messager.print("Making motion gallery with %d images" % (len(self.snaps)))
            with private_charts.open_for_write(page="motion_gallery") as page_stream:
                base = os.path.expanduser(private_charts.base)
                page_stream.write(
                    exprpages.page_text(
                        [T.body()[
                            T.h1["Motion gallery"],
                            T.p["There are %d images" % len(self.snaps)],
                            [[T.h2[image['created']],
                              T.a(href=image['video']['filename'])[T.img(src=local_copy(image['filename'], base))]
                              ]
                             for image in self.snaps]
                        ]],
                        style_text="",
                        script_text=""))

        super().update(verbose, messager)
        return self

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        messager.print("making HTML for motion detection with snap files %s" % self.snaps)
        return T.div[T.p["%d videos have been taken in the past day" % len(self.snaps)]]
