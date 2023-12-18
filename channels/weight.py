"""Example panel to copy and base new ones on."""

import datetime
import os
import shutil

import pandas as pd

import channels.panels as panels
from expressionive.expressionive import htmltags as T
from expressionive.expridioms import wrap_box, labelled_section, linked_image

import backup
import qsutils.qsmerge
import qsutils.check_merged_row_dates

class WeightPanel(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.physical_file = os.path.expandvars("$SYNCED/health/physical.csv")
        self.by_date = None
        self.dataframe = None

    def fetch(self, **kwargs):
        """Fetch data from external sources."""
        pass

    def update(self, **kwargs):
        """Update the cached data."""

        phys_scratch = "/tmp/physical-tmp.csv"

        physical_files = [os.path.expandvars("$SYNCED/health/weight.csv")]

        self.by_date = qsutils.qsmerge.qsmerge(
            self.physical_file,
            physical_files,
            None,
            phys_scratch)

        if qsutils.check_merged_row_dates.check_merged_row_dates(
                phys_scratch, self.physical_file, *physical_files):
            backup.backup(self.physical_file, "$HOME/archive", "physical-to-%s.csv")
            shutil.copy(phys_scratch, self.physical_file)
        else:
            if verbose:
                print("merge of physical data produced the wrong number of rows")

        self.updated = datetime.datetime.now()
        return self

    def prepare_page_images(self, date_suffix, begin_date, end_date, chart_sizes):
        """Prepare any images used by the output of the `html` method."""
        # TODO: rolling averages
        self.dataframe = pd.read_csv(self.physical_file)
        self.dataframe['Date'] = pd.to_datetime(self.dataframe['Date'])
        for units in ('stone', 'kilogram', 'pound'):
            qsutils.qschart.qscharts(
                data=self.dataframe,
                file_type='weight',
                timestamp=None,
                columns=[units],
                begin=begin_date, end=end_date, match=None,
                by_day_of_week=False, # split_by_DoW
                outfile_template=os.path.join(
                    self.charts_dir, "weight-%s-%s-%%s.png" % (units, date_suffix)),
                plot_param_sets=chart_sizes,
                vlines=None)

    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return T.div[wrap_box(
            linked_image(
                charts_dir=self.charts_dir,
                image_name="weight-stone",
                label="weight"))]

    def name(self):
        return "weight"

    def label(self):
        return "Weight"
