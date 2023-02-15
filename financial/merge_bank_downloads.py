#!/usr/bin/env python3

import argparse
import csv
import glob
import os
from frozendict import frozendict

import finutils

def merge_bank_downloads(transactions, downloads, account):
    """Add transactions from downloads for an account.

    The transactions are a list of dicts.

    The downloads are a list of lists of dicts."""
    for download in downloads:
        transactions |= {
            frozendict(raw)
            for raw in download
            if (raw['Narrative'] != "Opening Balance "
                and int(raw['Account']) == account)}
    return transactions

def bank_downloads(pattern):
    return [finutils.read_transactions(download)
            for download in glob.glob(os.path.expanduser(pattern))]

def merge_bank_download_files(base, account, output, pattern=finutils.UPDATES_GLOB):
    """Add to a transactions file from more recent downloads files for an account."""
    return finutils.write_csv(merge_bank_downloads(finutils.read_transactions(base),
                                                   bank_downloads(pattern),
                                                   int(account)),
                              finutils.BANK_COLUMNS,
                              output,
                              lambda r: (r['Value Date'], r['Narrative']))

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--account", "-a", type=int)
    parser.add_argument("--base", "-b", default=finutils.BANK_BASE)
    parser.add_argument("--output", "-o", default=finutils.BANK_FULL)
    return vars(parser.parse_args())

if __name__ == "__main__":
    merge_bank_download_files(**get_args())
