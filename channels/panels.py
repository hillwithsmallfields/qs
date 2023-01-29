import os
import sys

def ensure_in_path(directory):
    if directory not in sys.path:
        sys.path.append(directory)

source_dir = os.path.dirname(os.path.realpath(__file__))

# This corresponds to https://github.com/hillwithsmallfields
my_projects = os.path.dirname(os.path.dirname(source_dir))

ensure_in_path(os.path.dirname(source_dir))

import dashboard.dashboard
import lifehacking_config       # https://github.com/hillwithsmallfields/noticeboard/blob/master/lifehacking_config.py

ensure_in_path(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

def namify(x):
    return x.replace(' ', '_')

def make_name_with_email(name, email):
    return (T.a(href="email:"+email)[name]
            if email and email != ""
            else name)

def row(*things):
    """Returns an untemplated table row for its arguments."""
    return T.table(width="100%")[T.tr[[T.td(valign="top")[thing] for thing in things]]]

def wrap_box(*things):
    """Returns a flex container box contains its arguments."""
    return (T.div(class_='flex-container')[[T.div[thing]
                                            for thing in things
                                            if thing]]
            if any(things)
            else None)

def labelled_section(title, body):
    """Returns a titled version of the body."""
    return T.div[T.h2[title], body] if body else None

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

def linked_image(image_name, label, fallback=None):
    """Returns a group of image panels with the image of each linked to a larger version of itself."""
    charts_dir = lifehacking_config.file_config('general', 'charts')
    os.path.join(charts_dir)
    periods = ('all_time', 'past_year', 'past_quarter', 'past_month', 'past_week')
    return switchable_panel(
        label,
        panels={period: [
            T.div(class_='choice', name=period)[
                (T.a(href="%s-%s-large.png" % (image_name, period))[
                    T.img(src="%s-%s-small.png" % (image_name, period))]
                 if os.path.isfile(os.path.join(charts_dir, "%s-%s-small.png" % (image_name, period))) # TODO: this isn't right, is it looking in the right directory?
                 else fallback or T.p["Data needs fetching"])]
        ]
                for period in periods},
        labels={period: period.capitalize().replace('_', ' ') for period in periods},
        order=periods,
        initial='past_quarter')
