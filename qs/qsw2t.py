#!/usr/bin/python

# Program to go from my combined body logs to Weight Tracker app format.

# If an existing Weight Tracker file is given, any entries in it are
# skipped from the output, as the point of the program is to fill in
# gaps in the app's data from other data (e.g. from before I started
# to use the app).

import argparse
import csv

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-e", "--existing",
                        help="""The name of a file containing the existing app entries as exported by the app.""")
    parser.add_argument("-o", "--output",
                        help="""The name of the file to put the output in.""")
    parser.add_argument("data",
                        help="""The main data file.""")

if __name__ == "__main__":
    main()
