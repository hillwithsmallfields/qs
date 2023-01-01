#!/usr/bin/env python3

import argparse
import csv
import glob
import os
from frozendict import frozendict

import finutils

def merge_bank_downloads(base, account, output):
    bank_full = finutils.read_csv(base)
    bank_full_ts = os.path.getmtime(base)
    for download in glob.glob(finutils.UPDATES_GLOB):
        if os.path.getmtime(download) > bank_full_ts:
            bank_full |= {
                frozendict({
                    'Date': raw['Value Date'],
                    'Details': raw['Narrative'],
                    'Money out': raw['Dr Amount'],
                    'Money in': raw['Cr Amount'],
                    'Balance': raw['Balance'],
                })
                for raw in finutils.read_csv(download)
                if (raw['Narrative'] != "Opening Balance "
                    and int(raw['Account']) == account)}
    finutils.write_csv(bank_full, finutils.BANK_COLUMNS, output, lambda r: (r['Date'], r'[Details'))

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--account", "-a", type=int)
    parser.add_argument("--base", "-b", default=finutils.BANK_BASE)
    parser.add_argument("--output", "-o", default=finutils.BANK_FULL)
    return vars(parser.parse_args())

if __name__ == "__main__":
    merge_bank_downloads(**get_args())
