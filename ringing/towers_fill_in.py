#!/usr/bin/env python3
# Fill in details in my towers list, from Dove

import csv
import tower_visits

def towers_fill_in_main():
    tower_visits.download_dove()
    dove = tower_visits.read_dove()
    visits = tower_visits.read_visits()

    tower_visits.towers_fill_in(dove, visits)
    tower_visits.write_visits(visits)

    by_bells, by_weight, by_year = tower_visits.classify_towers(visits)

    with open("/tmp/by-weight.csv", 'w') as bw_stream:
        writer = csv.writer(bw_stream)
        writer.writerow(['Hundredweight', 'Towers'])
        for cwt in sorted(by_weight.keys(), reverse=True):
            writer.writerow([cwt, by_weight[cwt]])
    with open("/tmp/by-bells.csv", 'w') as bb_stream:
        writer = csv.writer(bb_stream)
        writer.writerow(['Bells', 'Towers'])
        for bells in sorted(by_bells.keys(), reverse=True):
            writer.writerow([bells, by_bells[bells]])
    with open("/tmp/by-year.csv", 'w') as by_stream:
        writer = csv.writer(by_stream)
        writer.writerow(['Year', 'Towers'])
        for year in sorted(by_year.keys(), reverse=True):
            writer.writerow([year, by_year[year]])

if __name__ == "__main__":
    towers_fill_in_main()
