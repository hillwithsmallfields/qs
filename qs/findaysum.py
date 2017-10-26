#!/usr/bin/python

# Program to sum each day's transactions per payee in finance spreadsheets

import argparse
import csv
import os
import yaml

# See notes in finconv.py for config file format

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config",
                        default="~/.qs-conf.yaml")
    parser.add_argument("-f", "--format",
                        default='combined')
    parser.add_argument("-o", "--output")
    parser.add_argument("input_file")
    args = parser.parse_args()

    with open(os.path.expanduser(os.path.expandvars(args.config))) as config_file:
        config = yaml.safe_load(config_file)

    print "config is", config

if __name__ == "__main__":
    main()
