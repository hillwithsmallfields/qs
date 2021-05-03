# Financial spreadsheet functions

import copy
import csv
import datetime
import os
import re

import account
import account_sheet
import base_sheet
import canonical_sheet
import categoriser
import classify
import diff_sheet
import filter_dates
import finlisp_evaluation
import formatted_sheet
import itemized_amount
import named_column_sheet
import parentage
import qsutils
import tracked_sheet

class ConfigRequired(BaseException):
    pass

    def __init__(self, function_name):
        self.function_name = function_name

class CannotConvert(BaseException):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name
        self.problematic_type = type(value)

class UnsupportedOperation(BaseException):
    pass

    def __init__(self, function_name, value):
        self.function_name = function_name
        self.problematic_type = type(value)
        self.function_name = function_name

functions = ['account_to_sheet',
             'add_row',
             'add_sheet',
             'add_sheets',
             'adjustments_by_day',
             'adjustments_by_month',
             'adjustments_by_year',
             'annotate_by_timestamp',
             'annotate_matches',
             'between_dates',
             'blank_sheet',
             'by_classification',
             'by_day',
             'by_hierarchy',
             'by_month',
             'by_parent',
             'by_year',
             'categories',
             'categorised',
             'chart',
             'check',
             'column_average',
             'column_average_absolute',
             'compare',
             'copy_sheet',      # temporary for debugging
             'count_day_categories',
             'count_day_transactions',
             'count_month_categories',
             'count_month_transactions',
             'count_year_categories',
             'count_year_transactions',
             'fgrep',
             'filter_sheet',
             'find_amount',
             'find_by_field',
             'first_of_month',
             'first_of_year',
             'flagged_as',
             'flagged_categories',
             'format_sheet',
             'grep',
             # 'join', # todo
             'join_by_days',
             'join_by_months',
             'join_by_years',
             'last_of_month',
             'last_of_year',
             'list_accounts',
             'make_update_sheet',
             'occupied_columns',
             'payees',
             'proportions',
             'read_classifier',
             'read_thresholds',
             'read_parentage_table',
             'remove_columns',
             'rename_column',
             'replace_matching_rows',
             'select_columns',
             'setq',
             'sheet',
             'show',
             'subtract_cells',
             'this_month',
             'this_year',
             'threshold',
             'track',
             'unclassified_categories',
             'write_all_columns',
             'write_csv',
             'write_csv_with_averages',
             'write_html',
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

def copy_sheet(context, sheet):
    return copy.copy(sheet)     # todo: use this as a base for filtering sheets to first / last transactions of each time period

def account_to_sheet(context, base_account):
    return account_sheet.account_sheet(base_account.config, base_account)

def add_row(context, sheet, row):
    sheet.add_row(row)
    return sheet

def add_sheet(context, base_account, sheet, flags=None, trace_sheet_name=None):
    """Add a sheet to an account or to another sheet."""
    if isinstance(base_account, account.account):
        return account.add_sheet(sheet, flags=flags, trace_sheet_name=trace_sheet_name)
    else:
        return base_account.add_sheet(sheet)

def add_sheets(context, *sheets):
    return sheets[0].add_sheets(*sheets[1:])

def adjustments_by_day(context, mainsheet, statements, account_name):
    return mainsheet.adjustments(statements, account_name, qsutils.granularity_day)

def adjustments_by_month(context, mainsheet, statements, account_name):
    return mainsheet.adjustments(statements, account_name, qsutils.granularity_month)

def adjustments_by_year(context, mainsheet, statements, account_name):
    return mainsheet.adjustments(statements, account_name, qsutils.granularity_year)

def annotate_by_timestamp(context, sheet, reference, annotation_columns):
    return filter_dates.annotate_by_timestamp(sheet, reference, annotation_columns)

def annotate_matches(context, sheet, reference):
    return sheet.annotate_matches(reference)

def between_dates(context, original, begin_date, end_date):
    return original.between_dates(begin_date, end_date)

def blank_sheet(context):
    return canonical_sheet.canonical_sheet(context['config'])

def classify_helper(row, parentage_table, classifiers, collect_unknowns, keep_unknowns):
    category = row['category']
    result = classify.classify(category,
                               parentage_table.get(category),
                               classifiers,
                               collect_unknowns=collect_unknowns,
                               pass_unknowns=keep_unknowns)
    return result

def by_classification(context, original, parentage_table, classifiers, collect_unknowns, keep_unknowns):
    return categorised_by_key_fn(context, original,
                                 lambda row: classify_helper(row, parentage_table, classifiers, collect_unknowns, keep_unknowns),
                                 label="by_classification")

def by_day(context, original, combine_categories, combined_only):
    return original.combine_same_period_entries(qsutils.granularity_day,
                                                combine_categories,
                                                combined_only,
                                                comment="Daily summary")

def row_parent(row, parentage_table):
    ancestry = parentage_table.get(row['category'])
    return ancestry[-1] if ancestry else None

def hierarchy_helper(row, depth, parentage_table):
    parent = row_parent(row, parentage_table)
    if parent:
        levels = (parent + ":" + row.get('category', "")).split(":")
        return levels[min(len(levels)-1, depth)]
    else:
        return row.get('category', "")

def by_hierarchy(context, original, depth, parentage_table):
    return categorised_by_key_fn(context, original,
                                 lambda row: hierarchy_helper(row, depth, parentage_table),
                                 label="by_hierarchy")

def by_month(context, original, combine_categories, combined_only):
    return original.combine_same_period_entries(qsutils.granularity_month,
                                                combine_categories,
                                                combined_only,
                                                comment="Monthly summary")

def by_parent(context, original, parentage_table):
    """Really meant for use on periodic summaries."""
    return categorised_by_key_fn(context, original,
                                 lambda row: row_parent(row, parentage_table),
                                 label="by_parent")

def by_year(context, original, combine_categories, combined_only):
    return original.combine_same_period_entries(qsutils.granularity_year,
                                                combine_categories,
                                                combined_only,
                                                comment="Yearly summary")

def categories(context, original):
    return categoriser.category_tree(original)

def categorised(context, original):
    """Really meant for use on periodic summaries."""
    return categorised_by_key_fn(context, original, lambda row: row['category'],
                                 label="categorised")

def categorised_by_key_fn(context, incoming_data, key_fn, label=""):
    """For each date occurring in the incoming data,
    use a key function to categorise the rows on that date,
    and collect an itemized amount for each such category.
    Really meant for use on periodic summaries."""
    categories = set()
    by_date = {}
    for timestamp, row in incoming_data.rows.items():
        on_day = timestamp.date()
        if on_day not in by_date:
            by_date[on_day] = {}
        day_accumulator = by_date[on_day]
        category = key_fn(row)
        original_amount = itemized_amount.as_number(row['amount'])
        row = row.copy()
        row['amount'] = original_amount
        categories.add(category or "unknown")
        amount = itemized_amount.itemized_amount(row)
        if category in day_accumulator:
            day_accumulator[category] += amount
        else:
            day_accumulator[category] = amount
    # print("ended categorised_by_key_fn")
    # print("bykey prepared rows:", label)
    # for k, v in by_date.items():
    #     print("bykey date    ", k)
    #     for catk, catv in v.items():
    #         print("bykey cat      ", catk, repr(catv), ["(%f:%s)" % (item.get('amount', "unknown amount").amount, item.get('payee', "unknown payee")) for item in catv.transactions])
    return named_column_sheet.named_column_sheet(incoming_data.config,
                                                 sorted(categories),
                                                 rows=by_date)

def chart(context, sheet, title, filename, fields):
    """Output a sheet to gnuplot."""
    sheet.chart(title, filename, fields)
    return fields

VERBOSE_CHECK = False

def check(context, label, sheet):
    print("checking", label)
    duplications = 0
    duplicates = 0
    missing_unique_numbers = 0
    items_absent = 0
    items_present = 0
    itemized = 0
    non_itemized = 0
    amounted = 0
    non_amounted = 0
    transaction_count_histogram = {}
    for row in sheet.rows.values():
        if 'item' in row and row['item'] != "":
            items_present += 1
        else:
            items_absent += 1
        if 'unique_number' not in row:
            missing_unique_numbers += 1
        if 'amount' in row:
            amounted += 1
            if isinstance(row['amount'], itemized_amount.itemized_amount):
                itemized += 1
                transactions = row['amount'].transactions
                tr_count = len(transactions)
                transaction_count_histogram[tr_count] = 1 + transaction_count_histogram.get(tr_count, 0)
            else:
                non_itemized += 1
        else:
            non_amounted += 1
        for amount in row.values():
            if isinstance(amount, itemized_amount.itemized_amount):
                dups = amount.count_duplicates()
                if dups:
                    duplications += 1
                    duplicates += dups
                    if VERBOSE_CHECK:
                        print("in check", label, "found", dups, "duplicates in", row)
                if isinstance(amount.amount, itemized_amount.itemized_amount):
                    if VERBOSE_CHECK:
                        print("in check", label, "nested amount: outer", repr(amount), "inner", repr(amount.amount), type(amount.amount.amount))
                    if isinstance(amount.amount.amount, itemized_amount.itemized_amount):
                        if VERBOSE_CHECK:
                            print("in check", label, "double nested amount: outer", repr(amount), "inner", repr(amount.amount))
                            print("in check", label, "itemized amounts:", itemized, "non_itemized:", non_itemized)
    print("checked", label, "duplications:", duplications, "duplicates:", duplicates, "missing unique numbers:", missing_unique_numbers, "amounts present:", amounted, "amounts absent:", non_amounted, "items present:", items_present, "items_absent:", items_absent)
    if transaction_count_histogram:
        print("in check", label, "itemized amount transaction list lengths:")
        for count in sorted(transaction_count_histogram.keys()):
            print("in check", label, "  ", count, transaction_count_histogram[count])
    else:
        print("in check", label, "No itemized amounts")
    return sheet

def column_average(context, sheet, colname):
    """Return the average value of the named column."""
    return sheet.column_average(colname, False)

def column_average_absolute(context, sheet, colname):
    """Return the average absolute value of the named column."""
    return sheet.column_average(colname, True)

def compare(context,
            result_column,
            sheet_a, column_a, track_a, filter_a_col, filter_a_val,
            sheet_b, column_b, track_b, filter_b_col, filter_b_val):
    return diff_sheet.diff_sheet(result_column,
                                 sheet_a, column_a,
                                 sheet_b, column_b,
                                 track_a=track_a, filter_a_col=filter_a_col, filter_a_val=filter_a_val,
                                 track_b=track_b, filter_b_col=filter_b_col, filter_b_val=filter_b_val)

def count_day_categories(context, sheet):
    return sheet.count_same_period_categories(qsutils.granularity_day)

def count_day_transactions(context, sheet):
    return filter_dates.count_by_dates(sheet, qsutils.granularity_day)

def count_month_categories(context, sheet):
    return sheet.count_same_period_categories(qsutils.granularity_month)

def count_month_transactions(context, sheet):
    return filter_dates.count_by_dates(sheet, qsutils.granularity_month)

def count_year_categories(context, sheet):
    return sheet.count_same_period_categories(qsutils.granularity_year)

def count_year_transactions(context, sheet):
    return filter_dates.count_by_dates(sheet, qsutils.granularity_year)

def filter_sheet(context, input_sheet, column, pattern):
    return input_sheet.filter_sheet(column, pattern)

def format_sheet(context, input_sheet, format_name):
    if input_sheet is None:
        return None
    if input_sheet.config is None:
        print("missing format in format_sheet")
        raise ConfigRequired("format_sheet")
    print("making formatted_sheet for format", format_name)
    return formatted_sheet.formatted_sheet(input_sheet.config,
                                           format_name,
                                           input_sheet)

def fgrep(context, input_sheet, match, column='payee'):
    if isinstance(input_sheet, base_sheet.base_sheet):
        result = copy.copy(input_sheet)
        result.rows = {k: v
                       for k, v in input_sheet.rows.items()
                       if v[column] == match}
        return result
    elif isinstance(input_sheet, account.account):
        pass                    # todo: not sure what to do with this
    else:
        print("Cannot fgrep a", type(input_sheet))
        raise UnsupportedOperation("fgrep", type(input_sheet))

def find_amount(context, sheet, amount, approx_date, within):
    return sheet.find_amount(amount, approx_date, within)

def get_conversions(context, formatname):
    return context['config']['formats'][formatname].get(
        'conversions',
        context['config']['formats']['Default'].get('conversions',
                                                    {}))
def flagged_as(context, formatname, payee, flag):
    conversions = get_conversions(context, formatname)
    payee_details = canonical_sheet.find_conversion(conversions, payee)
    if payee_details and 'flags' in payee_details:
        return flag in payee_details['flags']
    return False

def flagged_categories(context, formatname, flag):
    """Return a list of names of categories appearing in conversions for a given format with a given flag."""
    return list(set([details.get('category')
                     for details in get_conversions(context, formatname).values()
                     if ('category' in details
                         and 'flags' in details
                         and flag in details['flags'])]))

def safe_search(pattern, value):
    return pattern.search(value) if value else None

def grep(context, input_sheet, pattern, column='payee'):
    # TODO: filter accounts by payee
    if isinstance(input_sheet, base_sheet.base_sheet):
        result = object.__new__(input_sheet.__class__)
        result.__init__(input_sheet.config)
        pattern = re.compile(pattern)
        result.rows = {k: v
                       for k, v in input_sheet.rows.items()
                       if safe_search(pattern, v[column])}
        return result
    elif isinstance(input_sheet, account.account):
        pass
    else:
        print("Cannot grep a", type(input_sheet))
        raise UnsupportedOperation("grep", type(input_sheet))

def find_by_field(context, value, items, match_field):
    """Return one of a list of items that matches on a given field.
It matches if equal, a substring, or a superstring."""
    value_is_sequence = hasattr(value, '__len__')
    for item in items:
        item_value = item.get(match_field, None)
        if (item_value == value
            or (value_is_sequence and hasattr(item_value, '__len__')
                and (item_value in value
                     or value in item_value))):
            return item
    return False

def first_of_month(context, sheet):
    return filter_dates.filtered_by_date(sheet, qsutils.granularity_month, True)

def first_of_year(context, sheet):
    return filter_dates.filtered_by_date(sheet, qsutils.granularity_year, True)

def join_by_days(context, sheet_a, sheet_b):
    return filter_dates.join_by_dates(sheet_a, sheet_b, qsutils.granularity_day)

def join_by_months(context, sheet_a, sheet_b):
    return filter_dates.join_by_dates(sheet_a, sheet_b, qsutils.granularity_month)

def join_by_years(context, sheet_a, sheet_b):
    return filter_dates.join_by_dates(sheet_a, sheet_b, qsutils.granularity_year)

def last_of_month(context, sheet):
    return filter_dates.filtered_by_date(sheet, qsutils.granularity_month, False)

def last_of_year(context, sheet):
    return filter_dates.filtered_by_date(sheet, qsutils.granularity_year, False)

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

def make_update_sheet(context, sheet, reference):
    return sheet.make_update_sheet(reference)

def occupied_columns(context, sheet):
    """Return a copy of a sheet but with all empty columns removed."""
    return sheet.occupied_columns()

def read_parentage_table(context, parentage_filename):
    return parentage.read_parentage_table(parentage_filename
                                          if os.path.isabs(parentage_filename)
                                          else os.path.join(context['project_source'],
                                                            "conf",
                                                            parentage_filename))

def payees(context, original, pattern=None):
    return base_sheet.base_sheet(None,
                                 {name: {'payee': name,
                                         'total': details.transactions_total(),
                                         'transactions': details.transactions_string(separator='; ', time_chars=10)}
                                  for name, details in original.payees_matching(pattern).items()})

def proportions(context, original):
    return original.proportions()

def read_classifier(context, filename):
    return classify.read_classifier(filename
                                    if os.path.isabs(filename)
                                    else os.path.join(context['project_source'],
                                                      "conf",
                                                      filename))

def read_thresholds(context, filename):
    return classify.read_thresholds(context['config'], filename)

def remove_columns_row(timestamp, row, output_rows, colnames):
    output_rows[timestamp] = {colname: row[colname] for colname in row.keys() if colname not in colnames}

def remove_columns(context, sheet, column_names):
    _, output_rows = qsutils.process_rows(column_names, # app-data
                                          None, # input-format
                                          sheet.rows, # rows
                                          None, # setup
                                          remove_columns_row, # row handler
                                          None) # tidyup
    return named_column_sheet.named_column_sheet(sheet.config,
                                                 column_names,
                                                 rows=output_rows)

def rename_columns_row(timestamp, row, output_rows, names):
    output_rows[timestamp] = {names[1] if colname == names[0] else colname: cellvalue for colname, cellvalue in row.items()}

def rename_column(context, sheet, oldname, newname):
    _, output_rows = qsutils.process_rows([oldname, newname], # app-data
                                          None, # input-format
                                          sheet.rows, # rows
                                          None, # setup
                                          rename_columns_row, # row handler
                                          None) # tidyup
    return named_column_sheet.named_column_sheet(sheet.config,
                                                 [newname if name == oldname else name
                                                  for name in sheet.column_names],
                                                 rows=output_rows)

def replace_matching_rows(context, sheet, other, match_columns):
    return sheet.replace_matching_rows(other, match_columns)

def select_columns_row(timestamp, row, output_rows, colnames):
    try:
        output_rows[timestamp] = {colname: row[colname] for colname in colnames}
    except KeyError:
        print("Cannot select missing column; row:", row, "selected:", colnames)

def select_columns(context, sheet, column_names):
    _, output_rows = qsutils.process_rows(column_names, # app-data
                                          None, # input-format
                                          sheet.rows, # rows
                                          None, # setup
                                          select_columns_row, # row handler
                                          None) # tidyup
    return named_column_sheet.named_column_sheet(sheet.config,
                                                 column_names,
                                                 rows=output_rows)

def setq(context, name, value):
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

def subtract_cells(context, sheet, subtrahend):
    """Return a sheet with the cells of the second subtracted from the cells of the first."""
    return sheet.subtract_cells(subtrahend)

def this_month(context, original):
    return between_dates(context, original, datetime.date.today().replace(day=1), None)

def this_year(context, original):
    return between_dates(context, original, datetime.date.today().replace(month=1, day=1), None)

def threshold(context, sheet, threshold_level):
    return sheet.abs_threshold(threshold_level)

def track(context, sheet, tracked_column, tracking_column):
    return tracked_sheet.tracked_sheet(sheet.config, input_sheet=sheet,
                                       input_column=tracked_column,
                                       output_column=tracking_column)

def unclassified_categories(context, classifiers, parentage_table):
    return [category
            for category, parentage in parentage_table.items()
            if (category not in classifiers
                and not any([ancestor in classifiers for ancestor in parentage]))]

def write_all_columns(context, value, filename):
    if value:
        value.write_all_columns(qsutils.resolve_filename(
            filename,
            finlisp_evaluation.finlisp_var_value(context,
                                                 'output-dir')))
    else:
        print("Nothing to write to", filename)
    return value

def write_csv(context, value, filename):
    if value:
        value.write_csv(qsutils.resolve_filename(
            filename,
            finlisp_evaluation.finlisp_var_value(context,
                                                 'output-dir')),
                        suppress_timestamp=False)
    else:
        print("Nothing to write to", filename)
    return value

def write_csv_with_averages(context, value, filename, suppress_timestamp=False):
    if value:
        value.write_csv(qsutils.resolve_filename(
            filename,
            finlisp_evaluation.finlisp_var_value(context,
                                                 'output-dir')),
                        suppress_timestamp=suppress_timestamp,
                        show_averages=True)
    else:
        print("Nothing to write to", filename)
    return value

hovercss = '''
<style>
.overview {
  position: relative;
  display: inline-block;
}
.large {
  font-weight: bold;
}
.credit {
  color: green;
}
.ic {
  font-size: xx-small;
}
.details {
  visibility: hidden;
  background-color: yellow;
  z-index: 1;
  position: absolute;
}
.dethead {
    font-size: x-small;
}
.detdate {
    font-size: x-small;
}
.detamt {
    font-size: x-small;
}
.detpay {
    font-size: x-small;
}
.detcat {
    font-size: x-small;
}
.detitem {
    font-size: x-small;
}
.overview:hover .details {
  visibility: visible;
}
table.summarytable {
  border: 1px solid black;
  width: 100%;
}
.duplicated {
  color: red;
}
</style>
'''

def write_html(context, sheet, filename, title,
               thresholds=None, details=False,
               with_time=False,
               explanation=None):
    full_filename = qsutils.resolve_filename(
        filename,
        finlisp_evaluation.finlisp_var_value(context,
                                             'output-dir'))
    qsutils.ensure_directory_for_file(full_filename)
    with open(full_filename, 'w') as outstream:
        outstream.write('<html><head><title>%s</title></head>' % title)
        if details:
            outstream.write(hovercss)
        outstream.write('\n<body>\n')
        if explanation:
            outstream.write("\n<p>" + explanation + "</p>\n")
        sheet.write_html_table(outstream,
                               css_class="summarytable",
                               hover_details=details,
                               col_extra_data=thresholds,
                               with_time=with_time)
        outstream.write('</body></html>\n')

def write_json(context, value, filename):
    if value:
        value.write_json(qsutils.resolve_filename(
            filename,
            finlisp_evaluation.finlisp_var_value(context,
                                                 'output-dir')))
    else:
        print("Nothing to write to", filename)
    return value

def write_debug(context, value, filename):
    if value:
        value.write_debug(qsutils.resolve_filename(
            filename,
            finlisp_evaluation.finlisp_var_value(context,
                                                 'output-dir')))
    else:
        print("Nothing to write to", filename)
    return value
