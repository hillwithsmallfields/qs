import os
import sys

my_projects = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

ensure_in_path(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

ensure_in_path(os.path.join(my_projects, "noticeboard"))

import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

class Factotum:

    """The data and methods for my lifehacking system."""

    def __init__(self, config):
        self.config_data = config

    def config(self, *keys):
        result = lifehacking_config.config(*keys)
        if result is None:
            print("failed to get config", keys)
            print("entries", lifehacking_config.CONFIGURATION)
        return result

    def file_config(self, *keys):
        return os.path.expanduser(os.path.expandvars(self.config(*keys)))
