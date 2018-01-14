#!/usr/bin/python

# Program to split accounts into a separate column per category

import argparse
import csv
import os
import qsutils

# See notes in finconv.py for config file format

DEFAULT_CONF = "/usr/local/share/qs-accounts.yaml"

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        action='append')
    parser.add_argument("-n", "--no-default-config",
                        action='store_true',
                        help="""Do not load the default config file.""")
    parser.add_argument("-v", "--verbose",
                        action='store_true')
    parser.add_argument("-o", "--output")
    parser.add_argument("-f", "--format",
                        default=None)
    parser.add_argument("input_file")

    args = parser.parse_args()

    config_files = ([DEFAULT_CONF]
                    if os.path.exists(DEFAULT_CONF) and not args.no_default_config
                    else [])

    if args.config:
        config_files += args.config

    config = qsutils.load_config(args.verbose, *config_files)

    # todo: deduce format of input file; should normally be financisto, or have similar data

    # todo: scan input file making a new set of columns, one for each category, and using the category column to distribute the data among them
