#!/usr/bin/python3

# Program to chart my Quantified Self files.

import argparse
import csv
import datetime
import qsutils
import re
import tempfile

def handle_stones_row(row):
    return row['Stone']*14 + row['Lbs'] if 'Stone' in row and 'Lbs' in row else None

ROW_HANDLERS = {
    'weight': handle_stones_row,
    'stones': handle_stones_row
}

def row_filter(filter_control, row):
    if 'begin' in filter_control:
        if row['__DATE__'] < filter_control['begin']:
            return False
    if 'end' in filter_control:
        if row['__DATE__'] > filter_control['end']:
            return False
    if 'match' in filter_control:
        if not filter_control['regexp'].match(row[filter_control['match']]):
            return False
    return True

def parsetime(timestr):
    return datetime.datetime.strptime(timestr, "%Y-%m-%d")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--debug", action='store_true')
    parser.add_argument("-o", "--output")
    parser.add_argument("-t", "--type")
    parser.add_argument("-v", "--verbose", action='store_true')

    parser.add_argument("-b", "--begin")
    parser.add_argument("-e", "--end")

    parser.add_argument("-m", "--match", nargs=2)

    parser.add_argument("mainfile")
    args = parser.parse_args()
    if args.type is None:
        with open(args.mainfile) as csvheaderprobe:
            probereader = csv.reader(csvheaderprobe)
            for row in probereader:
                fieldnames = row
                break               # read only the first row
        file_type = qsutils.deduce_file_type_from_headers(fieldnames)
        if args.verbose:
            print("Deduced file type", file_type)
    else:
        file_type = args.type
    if file_type not in ROW_HANDLERS:
        print("No handler for type", file_type)
        return

    row_handler = ROW_HANDLERS[file_type]

    filter_control = {}
    if args.begin:
        filter_control['begin'] = parsetime(args.begin)
    if args.end:
        filter_control['end'] = parsetime(args.end)
    if args.match:
        filter_control['match'] = args.match[0]
        filter_control['regexp'] = re.compile(args.match[1])

    epoch = datetime.datetime.utcfromtimestamp(0)
    data_rows = {}

    wanted_columns =

    with open(args.mainfile) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            rowdate = parsetime(row['Date'])
            row['__DATE__'] = rowdate
            data_rows[rowdate] = row_handler(row)
    results = [ [ row[col] for col in wanted_columns]
                for row in [ data_rows[date]
                             for date in sorted(data_rows.keys())
                             if row_filter(filter_control, data_rows[date]) ] ]
    with open(tempfile.NamedTemporaryFile(), 'w') as datafile:
        dataname = datafile.name
        datawriter = csv.writer(datafile)
        for row in results:
            datawriter.writerow(row)

# set title "My weight, 80.74-104.42"
# set terminal png size 2560,1920
# set output "/tmp/all-weight-kg.png"
# set timefmt x "%Y-%m-%d"
# set xdata time
# set format x "%Y-%m"
# # set xrange [ "80.74":"104.42" ]
# set xtics rotate by 45 border offset 0,.5 out nomirror 2419200
# set ylabel "Kilograms"
# set y2label "Kg change in week"
# set ytics nomirror
# set y2tics
# set grid xtics
# set datafile separator ","
# plot "/tmp/plot-weight.dat" using 1:7 with line axes x1y1 lc 7 title "Weight", "/tmp/plot-weight.dat" using 1:12 with line axes x1y1 lc 1 title "Average (week)", "/tmp/plot-weight.dat" using 1:14 with line axes x1y2 lc 3 title "Change (average) in week"

if __name__ == "__main__":
    main()
