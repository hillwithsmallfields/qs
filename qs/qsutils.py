#!/usr/bin/python
# Common routines for my QS programs

import os
import pprint
import yaml

# based on https://stackoverflow.com/questions/3232943/update-value-of-a-nested-dictionary-of-varying-depth
def rec_update(d, u, i=""):
    # print i, "rec_update of", d, "with", u
    for k, v in u.iteritems():
        # print i, "  k =", k, "; v = ", v
        if isinstance(v, dict):
            # print i, "dict update with", v
            d[k] = rec_update(d.get(k, {}), v, "  ")
        elif isinstance(v, list):
            # print i, "list update with", v
            d[k] = d.get(k, []) + v
        else:
            # print i, "plain update to", v
            d[k] = v
    return d

def load_config(*config_files):
    config = {}
    for file in config_files:
        with open(os.path.expanduser(os.path.expandvars(file))) as config_file:
            more_config = yaml.safe_load(config_file)
            # print "adding", more_config, "to", config
            rec_update(config, more_config)
            # print "config is now", config
    print "Combined config is", yaml.dump(config)
    return config

def main():
    """Tests on the utilities"""
    a = {"one": 1, "two": 2, "three": 3, "teens": {"thirteen": 13, "fourteen": 14}, "listing": ["aon", "do", "tri"]}
    b = {"four": 4, "five": 5, "six": 6, "teens": {"fifteen": 15, "sixteen": 16}, "listing": ["caithair", "cuig", "se"]}
    print "a is", a
    print "b is", b
    rec_update(a, b, "")
    print "a is now", a
    with open("/home/jcgs/qsconf/accounts.yaml") as confile:
        config = yaml.safe_load(confile)
    with open("/home/jcgs/qsconf/conversions.yaml") as confile:
        conversions = yaml.safe_load(confile)
    print "config is:"
    print pprint.pformat(config)
    print "conversions are:"
    print pprint.pformat(conversions)
    rec_update(config, conversions)
    print "overall config is:"
    print pprint.pformat(config)

if __name__ == "__main__":
    main()
