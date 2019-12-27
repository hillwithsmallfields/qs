# Financial spreadsheet functions

import diff_sheet
import re

functions = ['compare',
             'list_accounts',
             'write_all_columns',
             'write_csv']

functions_regexp = re.compile(r"\b(" + "|".join(functions) + r")\(")
variables_regexp = re.compile(r"([(,]) (*[A-Za-z][A-Za-z0-9_ ]*) *([,)])")

def convert_to_Python(command):
    """Add package prefixes to a Python string."""
    return variables_regexp.sub(
        "\1variables['\2']\3",
        functions_regexp.sub(r"finfuns.\1(variables, ", command))

# The functions

def list_accounts(variables, filename):
    with open(filename, 'w') as outfile:
        for name in sorted(variables.keys()):
            outfile.write(name + "\n")

def write_csv(value, filename):
    value.write_csv(filename)

def write_all_columns(value, filename):
    value.write_all_columns(filename)

def compare(result_column,
            sheet_a, column_a, track_a,
            sheet_b, column_b, track_b):
    return diff_sheet.diff_sheet(result_column,
                                 sheet_a, column_a, track_a,
                                 sheet_b, column_b, track_b)
