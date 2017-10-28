# Common routines for my QS programs

import os
import yaml

# based on https://stackoverflow.com/questions/3232943/update-value-of-a-nested-dictionary-of-varying-depth
def rec_update(d, u, i=""):
    print i, "rec_update of", d, "with", u
    for k, v in u.iteritems():
        print i, "  k =", k, "; v = ", v
        if isinstance(v, dict):
            print i, "dict update with", v
            d[k] = rec_update(d.get(k, {}), v, "  ")
        elif isinstance(v, list):
            print i, "list update with", v
            d.get(k, []).append(v)
        else:
            print i, "plain update to", v
            d[k] = v
    return d

def load_config(*config_files):
    config = {}
    for file in config_files:
        with open(os.path.expanduser(os.path.expandvars(file))) as config_file:
            more_config = yaml.safe_load(config_file)
            print "adding", more_config, "to", config
            rec_update(config, more_config)
            print "config is now", config
    return config
