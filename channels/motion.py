#!/usr/bin/env python3

import os.path
import subprocess
import traceback

import channels.panels as panels
from expressionive.expressionive import htmltags as T
import expressionive.exprpages as exprpages

from motion_monitor import motion_monitor, managed_directory

SNAPS_DIRECTORY = "/tmp/snaps"

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

        def _snap(video_file):
            snap_name = os.path.join(SNAPS_DIRECTORY, os.path.splitext(os.path.split(video_file)[1])[0] + ".jpg")
            messager.print("making snapshot %s from video %s" % (snap_name, video_file))
            subprocess.run(["ffmpeg", "-i", video_file,
                            "-ss", "0:00:01",
                            "-frames:v", "1",
                            snap_name],
                           stdout=subprocess.DEVNULL,
                           stderr=subprocess.DEVNULL)
            return snap_name

        messager.print("in MotionPanel.fetch")
        if self.motion_clips_directory:
            try:
                self.video_files = managed_directory.recent_files_in_directory(self.motion_clips_directory)
            except Exception as e:
                messager.print("Got exception %s while getting recent motion clips" % e)
            if self.video_files:
                messager.print("Generating snap files from %d motion clips" % len(self.video_files))
                try:
                    self.snap_files = [_snap(filedesc['filename']) for filedesc in self.video_files]
                except Exception as e:
                    messager.print("Got exception %s while snapping frames from videos" % e)
                    traceback.print_exception(e)
            else:
                messager.print("No video files found")
        else:
            messager.print("No motion clips directory")

    def update(self, verbose=False, messager=None, private_charts=None, **kwargs):
        """Update the cached data."""
        messager.print("in MotionPanel.update with private_charts %s and snap files %s" % (private_charts, self.snap_files))
        if self.snap_files and private_charts:
            with private_charts.open_for_write(page="motion_gallery.html") as page_stream:
                page_stream.write(
                    exprpages.page_text(
                        [T.body()[
                            T.h1["Motion gallery"],
                            T.p["There are %d images" % len(self.snap_files)]
                    ]]))

        super().update(verbose, messager)
        return self

    def html(self, _messager=None):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div[T.p["%d videos have been taken in the past day" % len(self.snap_files)]]
