import csv

file_type_headers = {
    'weight': frozenset(['Date', 'Stone', 'Lbs', 'Kg']),
    'temperature': frozenset(['Date','Morning','Evening']),
    'peak_flow': frozenset(['Date', 'Morning pre', 'Morning post', 'Evening pre', 'Evening post']),
    'handelsbanken': frozenset(['Date', 'Details', '"Money out"', '"Money in"', 'Balance']),
    'financisto': frozenset(['date', 'time', 'account', 'amount', 'currency', 'original amount', 'original currency', 'category', 'parent', 'payee', 'location', 'project', 'note'])
}

def first_row(filename):
    with open(filename) as csvheaderprobe:
        probereader = csv.reader(csvheaderprobe)
        for row in probereader:
            return row

def deduce_file_type(filename):
    headers = frozenset(first_row(filename))
    return sorted([(file_type, len(headers.intersection(sample)))
                   for file_type, sample in file_type_headers.items()],
                  reverse=True,
                  key=lambda pair: pair[1])[0][0]
