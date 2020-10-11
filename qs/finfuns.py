# Financial spreadsheet functions

import account
import account_sheet
import base_sheet
import canonical_sheet
import categoriser
import csv
import datetime
import diff_sheet
import formatted_sheet
import qsutils
import re
import tracked_sheet

class ConfigRequired(Exception):
    pass

    def __init__(self, function_name):
        self.function_name = function_name

class CannotConvert(Exception):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name
        self.problematic_type = type(value)

functions = ['account_to_sheet',
             'add_sheet',
             'by_day',
             'by_month',
             'by_year',
             'categories',
             'chart',
             'compare',
             'filter_sheet',
             'format_sheet',
             # 'join', # todo
             'list_accounts',
             'payees',
             'select_columns',
             'set',
             'sheet',
             'show',
             'track',
             'write_all_columns',
             'write_csv',
             'write_json',
             'write_debug']

functions_regexp = re.compile(r"\b(" + "|".join(functions) + r")\(")
variables_regexp = re.compile(r"([(,]) *([A-Za-z][A-Za-z0-9_ ]+) *(?=[,)])")

def convert_to_Python(command):
    """Add package prefixes to a Python string."""
    return functions_regexp.sub(r"finfuns.\1(context, ",
                                variables_regexp.sub(
                                    r"\1context['variables']['\2']",command))

# The functions

def account_to_sheet(context, base_account):
    return account_sheet.account_sheet(base_account.config, base_account)

def add_sheet(context, account, sheet, flags=None, trace_sheet_name=None):
    return account.add_sheet(sheet, flags=flags, trace_sheet_name=trace_sheet_name)

def by_day(context, original):
    return original.combine_same_period_entries(qsutils.granularity_day,
                                                comment="Daily summary")

def by_month(context, original):
    return original.combine_same_period_entries(qsutils.granularity_month,
                                                comment="Monthly summary")

def by_year(context, original):
    return original.combine_same_period_entries(qsutils.granularity_year,
                                                comment="Yearly summary")

def categories(context, original):
    return categoriser.category_tree(original)

def chart(context, sheet, title, filename, fields):
    """Output a sheet to gnuplot."""
    sheet.chart(title, filename, fields)
    return fields

def compare(context,
            result_column,
            sheet_a, column_a, track_a,
            sheet_b, column_b, track_b):
    return diff_sheet.diff_sheet(result_column,
                                 sheet_a, column_a, track_a,
                                 sheet_b, column_b, track_b)

def filter_sheet(context, input_sheet, column, pattern):
    return input_sheet.filter_sheet(column, pattern)

def format_sheet(context, input_sheet, format_name):
    if input_sheet is None:
        return None
    if input_sheet.config is None:
        print("missing format in format_sheet")
        raise ConfigRequired("format_sheet")
    return formatted_sheet.formatted_sheet(input_sheet.config,
                                           format_name,
                                           input_sheet)

def list_accounts(context, filename=None):
    varnames = sorted(context['variables'].keys())
    if filename:
        with open(qsutils.resolve_filename(filename), 'w') as outfile:
            colseq = ['variable', 'type']
            writer = csv.writer(outfile, colseq)
            writer.writerow(colseq)
            for name in varnames:
                writer.writerow([name, type(context['variables'][name]).__name__])
    return varnames

def payees(context, original, pattern):
    return base_sheet.base_sheet(None,
                                 {name: {'payee': name,
                                         'total': details.transactions_total(),
                                         'transactions': details.transactions_string(separator='; ', time_chars=10)}
                                  for name, details in original.payees_matching(pattern).items()})

def select_columns_row(timestamp, row, output_rows, colnames):
    output_rows[timestamp] = {row[colname] for colname in colnames}

def select_columns(context, sheet, column_names):
    output_headers, output_rows = process_rows(column_names,
                                               None,
                                               None, # setup
                                               select_columns_row,
                                               None)

def set(context, name, value):
    if name in context['variables']:
        print("Overwriting", name)
    context['variables'][name] = value
    return value

def sheet(context, subject):
    """Convert any subject to a canonical sheet."""
    print("converting", subject, "to spreadsheet")
    if subject is None:
        return None
    elif isinstance(subject, account.account) or isinstance(subject, base_sheet.base_sheet):
        if subject.config is None:
            print("Config required in sheet")
            raise ConfigRequired("sheet")
        return canonical_sheet.canonical_sheet(subject.config, input_sheet=subject)
    elif isinstance(subject, categoriser.category_tree):
        print("converting category tree", subject, "to category sheet")
        res = categoriser.category_tree(subject)
        print("got", res)
        return res
    else:
        raise CannotConvert("sheet", subject)

def show(context, value, filename):
    """Output any of the types we handle, for debugging."""
    with open(qsutils.resolve_filename(filename), 'w') as output:
        output.write(str(value))
        
def track(context, sheet, tracked_column, tracking_column):
    return tracked_sheet.tracked_sheet(sheet.config, input_sheet=sheet,
                                       input_column=tracked_column,
                                       output_column=tracking_column)

def write_all_columns(context, value, filename):
    if value:
        value.write_all_columns(qsutils.resolve_filename(filename))
    else:
        print("Nothing to write to", filename)
    return value

def write_csv(context, value, filename):
    if value:
        value.write_csv(qsutils.resolve_filename(filename))
    else:
        print("Nothing to write to", filename)
    return value

def write_json(context, value, filename):
    if value:
        value.write_json(qsutils.resolve_filename(filename))
    else:
        print("Nothing to write to", filename)
    return value

def write_debug(context, value, filename):
    if value:
        value.write_debug(qsutils.resolve_filename(filename))
    else:
        print("Nothing to write to", filename)
    return value
