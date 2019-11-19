class cvs_sheet:
    """A spreadsheet with headers."""

    def __init__(header, rows):
        self.header = header
        self.rows = rows

    def read(filename):
        with open(os.path.expanduser(os.path.expandvars(filename))) as infile:
            input_format_name, header_row_number = qsutils.deduce_stream_format(infile, config, args.verbose)
            for i in range(1, header_row_number):
                dummy = infile.readline()
            # todo: store the format information
            self.rows = [{k:v for k,v in row0.iteritems() if k != ''} for row0 in csv.DictReader(infile)]

    def write(filename):
        pass
