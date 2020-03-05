# Financial spreadsheet functions

import account
import base_sheet
import canonical_sheet
import categoriser
import csv
import datetime
import diff_sheet
import formatted_sheet
import qsutils
import re

class ConfigRequired(Exception):
    pass

    def __init__(self, function_name):
        self.function_name = function_name

class CannotConvert(Exception):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name
        self.problematic_type = type(value)

functions = ['add_sheet',
             'by_day',
             'by_month',
             'by_year',
             'categories',
             'compare',
             'format_sheet',
             'list_accounts',
             'payees',
             'set',
             'sheet',
             'show',
             'summarize',
             'write_all_columns',
             'write_csv',
             'write_debug']

functions_regexp = re.compile(r"\b(" + "|".join(functions) + r")\(")
variables_regexp = re.compile(r"([(,]) *([A-Za-z][A-Za-z0-9_ ]+) *(?=[,)])")

def convert_to_Python(command):
    """Add package prefixes to a Python string."""
    return functions_regexp.sub(r"finfuns.\1(variables, ",
                                variables_regexp.sub(
                                    r"\1variables['\2']",command))

# The functions

def add_sheet(variables, account, sheet, flags=None, trace_sheet_name=None):
    return account.add_sheet(sheet, flags=flags, trace_sheet_name=trace_sheet_name)

def by_day(variables, original):
    return original.combine_same_period_entries(qsutils.granularity_day,
                                                time_chars=10,
                                                comment="Daily summary")

def by_month(variables, original):
    return original.combine_same_period_entries(qsutils.granularity_month,
                                                time_chars=7,
                                                comment="Monthly summary")

def by_year(variables, original):
    return original.combine_same_period_entries(qsutils.granularity_year,
                                                time_chars=4,
                                                comment="Yearly summary")

def categories(variables, original):
    return categoriser.CategoryTree(original)

def compare(variables,
            result_column,
            sheet_a, column_a, track_a,
            sheet_b, column_b, track_b):
    return diff_sheet.diff_sheet(result_column,
                                 sheet_a, column_a, track_a,
                                 sheet_b, column_b, track_b)

def format_sheet(variables, input_sheet, format_name):
    if input_sheet is None:
        return None
    if input_sheet.config is None:
        print("missing format in format_sheet")
        raise ConfigRequired("format_sheet")
    return formatted_sheet.formatted_sheet(input_sheet.config,
                                           format_name,
                                           input_sheet)

def list_accounts(variables, filename=None):
    varnames = sorted(variables.keys())
    if filename:
        with open(qsutils.resolve_filename(filename), 'w') as outfile:
            colseq = ['variable', 'type']
            writer = csv.writer(outfile, colseq)
            writer.writerow(colseq)
            for name in varnames:
                writer.writerow([name, type(variables[name]).__name__])
    return varnames

def payees(variables, original, pattern):
    return base_sheet.base_sheet(None,
                                 {name: {'payee': name,
                                         'total': details.transactions_total(),
                                         'transactions': details.transactions_string(separator='; ', time_chars=10)}
                                  for name, details in original.payees_matching(pattern).items()})

def set(variables, name, value):
    if name in variables:
        print("Overwriting", name)
    variables[name] = value
    return value

def sheet(variables, subject):
    """Convert an subject to a canonical sheet."""
    print("converting", subject, "to spreadsheet")
    if subject is None:
        return None
    elif isinstance(subject, account.account) or isinstance(subject, base_sheet.base_sheet):
        if subject.config is None:
            print("Config required in sheet")
            raise ConfigRequired("sheet")
        return canonical_sheet.canonical_sheet(subject.config, input_sheet=subject)
    elif isinstance(subject, categoriser.CategoryTree):
        print("converting category tree", subject, "to category sheet")
        res = categoriser.CategoryTree(subject)
        print("got", res)
        return res
    else:
        raise CannotConvert("sheet", subject)

def show(variables, value, filename):
    """Output any of the types we handle, for debugging."""
    with open(qsutils.resolve_filename(filename), 'w') as output:
        output.write(str(value))

def summarize(variables, categories):
    """Make transaction lists for every (parent) level of a transaction tree."""
    return categories.summarize(categories)

def write_all_columns(variables, value, filename):
    if value:
        value.write_all_columns(qsutils.resolve_filename(filename))
    else:
        print("Nothing to write to", filename)
    return value

def write_csv(variables, value, filename):
    if value:
        value.write_csv(qsutils.resolve_filename(filename))
    else:
        print("Nothing to write to", filename)
    return value

def write_debug(variables, value, filename):
    if value:
        value.write_debug(qsutils.resolve_filename(filename))
    else:
        print("Nothing to write to", filename)
    return value
