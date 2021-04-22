import csv

DEFAULT_FILE_TYPE_HEADERS = {
    'weight': frozenset(['Date', 'Stone', 'Lbs', 'Kg']),
    'temperature': frozenset(['Date','Morning','Evening']),
    'peak_flow': frozenset(['Date', 'Morning pre', 'Morning post', 'Evening pre', 'Evening post']),
    'physical': frozenset(['Date', 'Stone', 'Lbs', 'Date number', 'Lbs total', 'St total', 'Kg', 'Non-zero', 'Week avg lb', 'Week avg st', 'Week avg lb mod 14', 'Week avg Kg', 'Week avg diff lb', 'Week avg diff Kg', 'Week avg diff ratio', 'Percent lost in week',
                           'Resting pulse', 'Peak flow (a.m.)', 'Peak flow (p.m.)',
                           'Calories at gym', 'Swim', 'Plank',
                           'Elapsed time', 'Travel time', 'Max speed', 'Average speed', 'Trip distance',
                           'Run distance', 'Run time', 'Run average speed',
                           'Waist', 'Chest', 'Left upper arm', 'Right upper arm', 'Left forearm', 'Right forearm', 'Left thigh', 'Right thigh', 'Left calf', 'Right calf',
                           'Comment']),
    'handelsbanken': frozenset(['Date', 'Details', '"Money out"', '"Money in"', 'Balance']),
    'financisto': frozenset(['date', 'time', 'account', 'amount', 'currency', 'original amount', 'original currency', 'category', 'parent', 'payee', 'location', 'project', 'note'])
}

def first_row(filename):
    """Return the first row of a CSV file."""
    with open(filename) as csvheaderprobe:
        probereader = csv.reader(csvheaderprobe)
        for row in probereader:
            return row

def deduce_file_type(filename, file_type_headers=DEFAULT_FILE_TYPE_HEADERS):
    """Match the first row of a file against a set of samples."""
    headers = frozenset(first_row(filename))
    return sorted([(file_type, len(headers.intersection(sample)) / len(sample))
                   for file_type, sample in file_type_headers.items()],
                  reverse=True,
                  key=lambda pair: pair[1])[0][0]
