# Financial spreadsheet functions

import canonical_sheet
import csv
import datetime
import diff_sheet
import formatted_sheet
import qsutils
import re

functions = ['add_sheet',
             'by_day',
             'by_month',
             'by_year',
             'compare',
             'format_sheet',
             'list_accounts',
             'payees',
             'set',
             'sheet',
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

def compare(variables,
            result_column,
            sheet_a, column_a, track_a,
            sheet_b, column_b, track_b):
    return diff_sheet.diff_sheet(result_column,
                                 sheet_a, column_a, track_a,
                                 sheet_b, column_b, track_b)

def format_sheet(variables, input_sheet, format_name):
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

def payees(variables, original, output, pattern):
    with open(qsutils.resolve_filename(output), 'w') as outfile:
        colseq = ['payee', 'transactions']
        writer = csv.writer(outfile, colseq)
        writer.writerow(colseq)
        for name, details in original.payees_matching(pattern).items():
            writer.writerow([name, details.transactions_string(separator='; ', time_chars=10)])

def set(variables, name, value):
    if name in variables:
        print("Overwriting", name)
    variables[name] = value
    return value

def sheet(variables, account):
    """Convert an account to a canonical sheet."""
    can = canonical_sheet.canonical_sheet(account.config, input_sheet=account)
    return can

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
