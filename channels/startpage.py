import datetime
import os

class StartPage:

    def __init__(self, *args, **kwargs):
        super().__init__(*args)
        self.updated = None

    def update(self, read_externals, verbose):

        """Update my personal start page, for which the master is a YAML file."""

        startpage = self.facto.file_config('start-page', 'startpage')
        startpage_source = self.facto.file_config('start-page', 'startpage-source')
        startpage_style = self.facto.file_config('start-page', 'startpage-style')
        if ((not os.path.isfile(startpage))
            or os.path.getmtime(startpage_source) > os.path.getmtime(startpage)
            or os.path.getmtime(startpage_style) > os.path.getmtime(startpage)):
            os.system("%s --output %s --stylesheet %s %s"
                      % (self.facto.file_config('start-page', 'start-page-generator'), startpage, startpage_style, startpage_source))

        self.updated = datetime.datetime.now()
        return self
