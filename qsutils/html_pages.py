import os
import sys

source_dir = os.path.dirname(os.path.realpath(__file__))

my_projects = os.path.dirname(os.path.dirname(source_dir))

sys.path.append(os.path.join(my_projects, "makers", "untemplate"))

import throw_out_your_templates_p3 as untemplate
from throw_out_your_templates_p3 import htmltags as T

def page_text(page_contents, style_text, script_text):
    return untemplate.Serializer(untemplate.examples_vmap, 'utf-8').serialize(
        untemplate.HTML5Doc([untemplate.safe_unicode(style_text
                                                     + script_text),
                             page_contents],
                            head=T.head[T.meta(charset='utf-8'),
                                        T.meta(rel='stylesheet',
                                               type_='text/css',
                                               href="dashboard.css"),
                                        T.title["Personal dashboard"]]))

def file_contents(filename):
    if os.path.isfile(filename):
        with open(filename) as instream:
            return instream.read()
    return "File %s not found" % filename

def tagged(tag, text):
    return "<" + tag + ">" + text + "</" + tag + ">"

def tagged_file_contents(tag, filename):
    return tagged(tag, file_contents(filename))
