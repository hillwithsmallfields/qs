import datetime
import tabular_page_maker.make_link_table
import os

import channels.panels as panels
import dobishem

class StartPage(panels.DashboardPanel):

    def __init__(self, *args, **kwargs):
        self.input_files = set(("startpage.yaml", "startpage.css"))
        super().__init__(*args, **kwargs)

    def label(self):
        return ""

    def reads_files(self, filenames):
        return filenames & self.input_files

    def update(self, verbose=False, messager=None):
        """Update my personal start page, for which the master is a YAML file."""
        startpage = os.path.expandvars("$HOME/private_html/startpage.html")
        startpage_source = os.path.expandvars("$SYNCED/org/startpage.yaml")
        startpage_style = os.path.expandvars("$SYNCED/org/startpage.css")
        if ((not os.path.isfile(startpage))
            or os.path.getmtime(startpage_source) > os.path.getmtime(startpage)
            or os.path.getmtime(startpage_style) > os.path.getmtime(startpage)):
            if verbose:
                if messager:
                    messager.print("Updating start page")
                else:
                    print("Updating start page")
            tabular_page_maker.make_link_table.output_table(dobishem.storage.load(startpage_source),
                                                            stylesheet=startpage_style,
                                                            output=startpage)

        super().update(verbose, messager)
        return self

    def html(self):
        """Generate an expressionive HTML structure from the cached data."""
        return []
