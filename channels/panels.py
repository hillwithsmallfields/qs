import os
import sys

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

source_dir = os.path.dirname(os.path.realpath(__file__))

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

ensure_in_path(os.path.dirname(source_dir))

ensure_in_path(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

def switchable_panel(switcher_id, panels, labels, order, initial):
    """Return a group of panels, only one of which is displayed at a time.

    - panels is a dictionary binding keys to the panel contents,
    - labels is a dictionary binding the same keys to button labels.
    - order is the order of the keys in the button row,
    - initial is the button to be selected initially.
    """
    return T.table(class_='switcher', id_=switcher_id)[
        T.tr(align="center")[T.td[[T.div(class_='choice', name=choice)[panels[choice]]
                                   for choice in order]]],
        T.tr(align="center")[
            T.td[[[T.button(class_=('active'
                                    if choice == initial
                                    else 'inactive'),
                            name=choice, onclick="select_version('%s', '%s')"%(switcher_id, choice))[labels[choice]]]
                  for choice in order]]]]
