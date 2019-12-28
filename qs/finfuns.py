# Financial spreadsheet functions

import diff_sheet
import re

functions = ['compare',
             'list_accounts',
             'write_all_columns',
             'write_csv']

functions_regexp = re.compile(r"\b(" + "|".join(functions) + r")\(")
variables_regexp = re.compile(r"([(,]) *([A-Za-z][A-Za-z0-9_ ]+) *([,)])")

def convert_to_Python(command):
    """Add package prefixes to a Python string."""
    return functions_regexp.sub(r"finfuns.\1(variables, ",
                                variables_regexp.sub(
                                    r"\1variables['\2']\3",command))

# The functions

def list_accounts(variables, filename=None):
    varnames = sorted(variables.keys())
    if filename:
        with open(filename, 'w') as outfile:
            for name in varnames:
                outfile.write(name + "\n")
    return varnames

def set(variables, name, value):
    variables[name] = value
    return value

def write_csv(variables, value, filename):
    value.write_csv(filename)
    return value

def write_all_columns(variables, value, filename):
    value.write_all_columns(filename)
    return value

def compare(variables,
            result_column,
            sheet_a, column_a, track_a,
            sheet_b, column_b, track_b):
    return diff_sheet.diff_sheet(result_column,
                                 sheet_a, column_a, track_a,
                                 sheet_b, column_b, track_b)
