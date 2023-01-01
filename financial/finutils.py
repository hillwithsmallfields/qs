import csv
import os.path
from frozendict import frozendict

def read_csv(filename):
    with open(os.path.expanduser(filename)) as instream:
        return set(frozendict(row) for row in csv.DictReader(instream))

def write_csv(data, header, filename):
    with open(os.path.expanduser(filename), 'w') as outstream:
        writer = csv.DictWriter(outstream, fieldnames=header)
        writer.writeheader()
        for row in sorted(data, key=lambda r: (r['Date'], r'[Details')):
            writer.writerow(row)

def read_yaml(filename):
    with open(os.path.expanduser(filename)) as instream:
        return yaml.safe_load(instream)

def show_sample(table, n_samples=12):
    for i in range(0, len(table), len(table)//n_samples):
        print(i, table[i])
