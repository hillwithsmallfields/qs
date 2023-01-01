#!/usr/bin/env python3

import argparse

import finutils

def list_values(table, column):
    return set(row[column] for row in table)

def list_values_in_file(incoming, column):
    for v in sorted(list_values(finutils.read_csv(incoming), column)):
        print(v)

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--incoming", "-i", default=finutils.MAIN_ACCOUNTING_FILE)
    parser.add_argument("--column", "-c", default='category')
    return vars(parser.parse_args())

if __name__ == "__main__":
    list_values_in_file(**get_args())
