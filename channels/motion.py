#!/usr/bin/env python3

import os.path
import shutil
import subprocess
import traceback

import channels.panels as panels
from expressionive.expressionive import htmltags as T
import expressionive.exprpages as exprpages

from motion_monitor import motion_monitor, managed_directory

SNAPS_DIRECTORY = "/tmp/snaps"

def local_copy(original, directory):
    short = os.path.split(original)[1]
    copy = os.path.join(directory, short)
    os.makedirs(directory, exist_ok=True)
    shutil.copyfile(original, copy)
    print("copied", original, "to", copy, "and called it", short)
    return short

class MotionPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        self.motion_clips_directory = None
        self.video_files = None
        self.snap_files = None
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
                self.video_files = managed_directory.recent_files_in_directory(self.motion_clips_directory,
                                                                               matching=lambda f: f.endswith(".mkv"))
                messager.print("got video files %s..." % self.video_files[:4])
                self.snap_files = managed_directory.recent_files_in_directory(self.motion_clips_directory,
                                                                              matching=lambda f: f.endswith(".jpg"))
                
                messager.print("got snap files %s" % self.snap_files)
            except Exception as e:
                messager.print("Got exception %s while getting recent motion clips and snaps" % e)
        else:
            messager.print("No motion clips directory")

    def update(self, verbose=False, messager=None, private_charts=None, **kwargs):
        """Update the cached data."""
        messager.print("in MotionPanel.update with private_charts %s and snap files %s" % (private_charts, self.snap_files))
        if self.snap_files and private_charts:
            with private_charts.open_for_write(page="motion_gallery") as page_stream:
                page_stream.write(
                    exprpages.page_text(
                        [T.body()[
                            T.h1["Motion gallery"],
                            T.p["There are %d images" % len(self.snap_files)],
                            [[T.h2[image['created']],
                              T.img(src=local_copy(image['filename'], private_charts.base))
                              ]
                             for image in self.snap_files]
                        ]],
                        style_text="",
                        script_text=""))

        super().update(verbose, messager)
        return self

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        messager.print("making HTML for motion detection with snap files %s" % self.snap_files)
        return T.div[T.p["%d videos have been taken in the past day" % len(self.snap_files)]]
