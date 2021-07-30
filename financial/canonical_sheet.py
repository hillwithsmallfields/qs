#!/usr/bin/python3

import copy
import csv
import os
import re

import sys

import account
import base_sheet
import csv_sheet
import itemized_amount
import named_column_sheet
import qsutils

# Convert sheets between a canonical format and specified other
# formats.  This may involve renaming columns, and filling in default
# values derived from the payee.

def templated_name(template, name):
    return (name
            if template is None or template == "%s"
            else (template
                  if "%s" not in template
                  else template % name))

def find_conversion(conversions, payee_name):
    """Find a mapping from the input format to the output, for a named payee."""
    if not conversions:
        return None
    for key, value in conversions.items():
        if (key == payee_name
            or value.get('payee') == payee_name
            or (payee_name
                and hasattr(key, '__len__')
                and (key in payee_name or payee_name in key))):
            return value
    return None

class canonical_sheet(base_sheet.base_sheet):
    """A financial data-only spreadsheet with a standard set of column names.

    This is the result of reading in a CSV file from a finance
    tracking app or a bank statement, and converting column names and
    translating payee names between naming schemes.
    """

    # not subclassed from csv_sheet, because that has a 'format' and
    # the point of this class is to avoid that.

    canonical_column_sequence_no_timestamp = [
        'date',
        'time',
        'account',
        'amount',
        'currency',
        'original_amount',
        'original_currency',
        'balance',
        'statement',
        'payee',
        'category',
        'project',
        'item',
        'message',
        # todo: possibly replace combicount with itemized_amounts?
        'combicount'            # for combining rows
    ]

    canonical_column_sequence = ['timestamp',
                                 'original_filename',
                                 'unique_number'] + canonical_column_sequence_no_timestamp

    def __init__(self,
                 config,
                 input_sheet=None,
                 convert_all=False,
                 account_name_template=None,
                 reference_sheet=None,
                 origin_files=[],
                 verbose=False):
        super().__init__(config)
        self.verbose = verbose
        if self.verbose:
            print("Making canonical_sheet with input_sheet", input_sheet)
        self.row_order = None
        self.row_cursor = 0
        self.by_days = {}
        self.origin_files = origin_files
        self.original_account_names = set()
        self.original_format_name = None
        self.currency_conversion = None
        if isinstance(input_sheet, str):
            if self.verbose:
                print("Reading", input_sheet, "for conversion")
            input_sheet = csv_sheet.csv_sheet(config,
                                              input_filename=input_sheet,
                                              verbose=self.verbose)
            print("input sheet format name is", input_sheet.format_name)
            self.original_format_name = input_sheet.format_name
        if isinstance(input_sheet, csv_sheet.csv_sheet):
            self.config = qsutils.combine_configs(input_sheet.config, config)
            self.origin_files = input_sheet.origin_files
            if input_sheet.currency_conversion:
                self.currency_conversion = re.compile(input_sheet.currency_conversion)
            if self.verbose:
                print("converting", input_sheet,
                      "with format name", input_sheet.format_name)
            if 'accounts' not in input_sheet.format:
                input_sheet.format['accounts'] = set()
            accounts = input_sheet.format['accounts']
            conversions = input_sheet.format.get(
                'conversions',
                self.config['formats']['Default'].get('conversions',
                                                      {}))
            for in_row in input_sheet:
                original_account = in_row.get('account', None)
                accounts.add(original_account)
                can_row, is_new = self.row_to_canonical(
                    input_sheet, in_row,
                    reference_sheet=self,
                    account_name_template=account_name_template,
                    conversions=conversions,
                    convert_all=convert_all)
                if self.verbose:
                    print("made", can_row, "from", in_row)
                if can_row:
                    if is_new:
                        if self.verbose:
                            print("storing", can_row)
                        self.rows[can_row['timestamp']] = can_row
                        self.by_days[can_row['date']] = can_row
                    else:
                        if self.verbose:
                            print("skipping row as duplicate", can_row)
        elif isinstance(input_sheet, canonical_sheet):
            self.origin_files = input_sheet.origin_files
            # take a copy
            self.rows = {k: {vk: vv for vk, vv in v.items()} for k, v in input_sheet.rows.items()}
        elif isinstance(input_sheet, account.account):
            self.config = qsutils.combine_configs(input_sheet.config, config)
            for payee in input_sheet:
                for timestamp, row in payee:
                    adjusted_timestamp = self.unused_timestamp_from(timestamp)
                    row['timestamp'] = adjusted_timestamp
                    row['date'] = adjusted_timestamp.date()
                    row['time'] = adjusted_timestamp.time()
                    self.rows[adjusted_timestamp] = row
        elif type(input_sheet) == dict:
            self.rows = input_sheet
            if not self.config and len(input_sheet) > 0:
                for sample in input_sheet.values():
                    if 'sheet' in sample:
                        self.config = sample['sheet'].config
                        break
        if self.verbose:
            print("finished making canonical sheet from", input_sheet)
        # print("finished initting canonical_sheet, dict binds", sorted(self.__dict__.keys()))

    def __iter__(self):
        self.row_order = sorted(self.rows.keys())
        self.row_cursor = -1    # because we pre-increment it
        return self

    def __next__(self):
        self.row_cursor += 1
        if self.row_cursor >= len(self.row_order):
            raise StopIteration
        return self.rows[self.row_order[self.row_cursor]]

    def __len__(self):
        return len(self.rows)

    def __str__(self):
        return ("<canonical spreadsheet with "
                + str(len(self.rows)) + " rows>")

    # def __enter__(self):
    #     # TODO: read file, keep filename
    #     pass

    # def __exit__(self):
    #     # TODO: write back to original file, leaving backup copy
    #     pass

    def column_names_list(self):
        return canonical_sheet.canonical_column_sequence

    def get_row_timestamp(self, row):
        return row.get('timestamp', None)

    def add_sheet(self, other):
        """Combine another sheet with this one."""
        for row in other.rows.values():
            self.add_row(row)
        return self

    def add_sheets(self, *others):
        """Combine several sheets, starting with this one.
        The original sheets are not affected."""
        print("adding to sheet", self, "these sheets:", others)
        result = canonical_sheet(self.config)
        result.add_sheet(self)
        for other in others:
            result.add_sheet(other)
        print("result of adding sheets is", result)
        return result

    def replace_matching_rows(self, other, match_columns):
        """Produce a sheet based on the present one but with rows substituted from another sheet.
        The rows to be replaced must match in the two sheets in all the specified columns.
        The original sheets are not altered."""
        # for row in other.rows.values():
        #     print("replacer row:", row)
        #     print("replacer key:", [str(row[col]) for col in match_columns])
        replacers = {
            tuple((row[col] for col in match_columns)): row
            for row in other.rows.values()
        }
        print("replacers are:", replacers)
        result = canonical_sheet(self.config)
        result.rows = {timestamp: replacers.get(tuple((row[col] for col in match_columns)), row)
                       for timestamp, row in self.rows.items()}
        return result

    def subtract_cells(self, other):
        """Return the cell-by-cell subtraction of another sheet from this one."""
        result = canonical_sheet(self.config)
        result.rows = {date: qsutils.subtracted_row(row,
                                            other.rows.get(date, {}),
                                            self.column_names_list())
                       for date, row in self.rows.items()}
        return result

    def abs_threshold(self, threshold):
        """Return a sheet like this but with any entries smaller than a given threshold omitted."""
        result = canonical_sheet(self.config)
        result.rows = {date: qsutils.thresholded_row(row, threshold)
                       for date, row in self.rows.items()}
        return result

    def row_to_canonical(self,
                         input_sheet, row,
                         convert_all=False,
                         conversions=None,
                         out_column_defaults=None,
                         reference_sheet=None,
                         account_name_template=None,
                         message=None):
        """Convert an input row from its own format to our standard format.
        If convert_all is False, convert only the rows with payees for whom
        the input sheet's format configuration has a conversion entry."""
        if reference_sheet is None:
            reference_sheet = input_sheet
        input_format = input_sheet.format
        in_columns = input_format['columns']
        row_date = qsutils.normalize_date(input_sheet.get_cell(row, 'date', None))
        if row_date is None:
            if self.verbose:
                print("empty date from row", row)
            return None, False
        money_in = input_sheet.get_numeric_cell(row, 'credits', 0)
        money_out = input_sheet.get_numeric_cell(row, 'debits', 0)
        amount = money_in - money_out
        original_amount = money_in - money_out
        original_currency = row.get('original_currency',
                                    out_column_defaults.get('original_currency', "GBP")
                                    if out_column_defaults
                                    else "GBP")
        payee_name = input_sheet.get_cell(row, 'payee', None)
        if payee_name is None:
            if self.verbose:
                print("payee field missing from row", row)
            return None, False
        if self.currency_conversion:
            curr_conv = self.currency_conversion.match(payee_name)
            if curr_conv:
                payee_name = curr_conv.group(1).strip()
                original_currency = curr_conv.group(2)
                original_amount = float(curr_conv.group(3))
        conversion = find_conversion(conversions if len(conversions) > 0 else input_format.get('conversions', {}),
                                     payee_name)
        if conversion is None and not convert_all:
            if self.verbose:
                print("no conversion for row", row)
            return None, False
        row_time = input_sheet.get_cell(
            row, 'time', ("01:02:03"
                          if out_column_defaults is None
                          else out_column_defaults.get('time', "01:02:03")))
        out_row = {
            'date': row_date,
            'time': row_time,
            # we don't bump the timestamp to be unique yet, to give
            # the new row a chance to be identical to an earlier
            # attempt, if we are fed exactly the same thing twice.
            'timestamp': reference_sheet.timestamp_from(row_date, row_time),
            'amount': amount,
            'account': templated_name(account_name_template,
                                      (input_sheet.get_cell(row, 'account', None)
                                       or input_format.get('name', "Unknown"))),
            'currency': row.get('currency',
                                input_format.get('currency', "?")),
            'original_amount': original_amount,
            'original_currency': original_currency,
            'sheet': self}
        for column in ('original_row_number', 'original_filename'):
            if column in row:
                out_row[column] = row[column]
        if message:
            out_row['message'] = message
        # For this group of columns, there may be some literals in
        # the "conversions" (payee descriptions) in the format
        # description.  This is how payee names are translated
        # from the naming scheme of the input sheet to that of the
        # output sheet.
        for canonical_outcol in ['balance', 'category',
                                 'payee', 'project', 'item', 'message']:
            # does the canonically named column have a default output value?
            if conversion and canonical_outcol in conversion:
                out_row[canonical_outcol] = conversion[canonical_outcol]
            # otherwise, can we copy it from an input cell?
            elif canonical_outcol in in_columns:
                out_row[canonical_outcol] = input_sheet.get_cell(row, canonical_outcol)
            # otherwise, are we given this as a default by our caller?
            elif (out_column_defaults is not None
                  and canonical_outcol in out_column_defaults):
                extra_value = out_column_defaults[canonical_outcol]
                # the join is initially for financisto's category parents:
                out_row[canonical_outcol] = (':'.join(extra_value)
                                             if isinstance(extra_value, list)
                                             else extra_value)
        # print("looking for flags in conversion", conversion)
        if conversion and 'flags' in conversion:
            out_row['flags'] = set(conversion['flags'].split())
        if self.rows.get(out_row['timestamp'], None) == out_row:
            # we have just created an exact duplicate of an existing row
            return out_row, False
        if row_date in self.by_days:
            # might be a duplicate with the same date but a different time
            for other in self.by_days[row_date]:
                same = True
                for colname in ('amount', 'payee', 'category'):
                    if other[colname] != out_row[colname]:
                        same = False
                        break
                if same:
                    return out_row, False
        # not a duplicate, so give it a unique timestamp so it can't
        # overwrite an existing (but not identical) row with the same
        # timestamp
        distinct = reference_sheet.unused_timestamp_from(row_date, row_time)
        out_row['timestamp'] = distinct
        out_row['time'] = distinct.time()
        out_row['date'] = distinct.date()
        return out_row, True

    def row_from_canonical(self, output_format, canonical_row, reverse_equivalents=None):
        """Convert a row from our standard format to a specified one."""
        if 'accounts' in output_format:
            print("output format has account names", output_format['accounts'])
        column_defaults = output_format.get('column-defaults', {})
        columns = output_format['columns']
        amount = canonical_row.get('amount', 0)
        if 'credits' in columns:
            if 'debits' in columns:
                canonical_row['credits' if amount > 0 else 'debits'] = amount
            else:
                canonical_row['credits'] = amount
        result = {output_column_name: canonical_row.get(canonical_column_name,
                                                        column_defaults.get(canonical_column_name, ""))
                  for canonical_column_name, output_column_name
                  in columns.items()}

        if reverse_equivalents:
            if result['account'] in reverse_equivalents:
               result['account'] = reverse_equivalents[result['account']]

        if ((result['category'] == 'Transfer')
            and 'transfer-handling' in output_format):
            for column, text in output_format['transfer-handling']['credit' if amount > 0 else 'debit'].items():
                result[column] = text

        if 'original currency' in result:
            if result['original currency'] == result['currency']:
                result['original currency'] = ""
        if 'original amount' in result:
            if result['original amount'] == result['amount']:
                result['original amount'] = ""

        return result

    def distribute_to_accounts(self, accounts={}, added_row_lists={}):
        """Distribute the rows of the sheet to account values.
        Returns a dictionary of accounts, and a dictionary of the rows
        that were added this time.
        Initial values for the dictionaries may be passed in."""
        for row in self.rows.values():
            account_name = row['account']
            if account_name not in accounts:
                accounts[account_name] = account.account(account_name)
            added_row, why_not = accounts[account_name].add_row_if_new(row)
            if added_row:
                if account_name not in added_row_lists:
                    added_row_lists[account_name] = []
                added_row_lists[account_name].append(added_row)
        return accounts, added_row_lists

    def find_amount(self, amount, timestamp, within_days, payee_hint=None):
        """Find entries with a given amount, around a given time."""
        possibilities = [row for row in self.rows.values()
                        if (abs(row['amount']) == abs(amount)
                             and qsutils.within_days(row['timestamp'], timestamp, within_days))]
        if payee_hint:
            filtered = [row
                        for row in possibilities
                        if row['payee'] == payee_hint]
            if filtered:
                possibilities = filtered
        return possibilities

    def combine_same_period_entries(self,
                                    period,
                                    combine_categories=True,
                                    combined_only=False,
                                    comment=None):
        """Produce a sheet based on this one, with just one entry per payee
        per period (day, by default).

        `period' is a function which should return the starting
        datetime.datetime of the period containing the date it is
        given.

        combine_categories means to collect up all the categories of
        the same payee.

        combined_only means to return only the rows that resulted from
        combining rows, dropping any that went straight through from
        individual incoming rows.
        """
        result = copy.copy(self)
        result.rows = {}
        accumulators = {}       # maps dates to maps of payee to combined transactions
        # print("combine_same_period_entries", period, "combine_categories", combine_categories, "combined_only", combined_only)
        for timestamp in self.rows.keys():
            row = self.rows[timestamp]
            row_date = period(timestamp)
            if row_date not in accumulators:
                accumulators[row_date] = {}
            this_period_by_payee = accumulators[row_date]
            try:
                payee_key = ((row['account'], row['payee'])
                             if combine_categories
                             else (row['account'], row['payee'], row['category']))
            except KeyError:
                print("required key missing in", row)
                raise(KeyError)
            if payee_key in this_period_by_payee:
                this_period_by_payee[payee_key] += row
            else:
                this_period_by_payee[payee_key] = itemized_amount.itemized_amount(row)
        if False:
            for timestamp in sorted(accumulators.keys()):
                summaries = accumulators[timestamp]
                print("cspe date ", timestamp)
                for summarykey in sorted(summaries.keys()):
                    print("cspe sum   ", summarykey, repr(summaries[summarykey]))
                    for sub in summaries[summarykey].transactions:
                        print("cspe sub       ", itemized_amount.row_descr(sub))
        unique_number = 0
        debug = False
        for timestamp, summaries in accumulators.items():
            for sumkey in summaries:
                summary = summaries[sumkey]
                if len(summary.transactions) == 1 and combined_only:
                    continue
                adjusted_datetime = result.unused_timestamp_from(timestamp)
                desc = ", ".join([itemized_amount.row_descr(x) for x in summary.transactions])
                # print("making row at", adjusted_datetime, "from", summary, "which is", desc)
                # print("summary.transactions is", summary.transactions)
                result.rows[adjusted_datetime] = {
                    'amount': summary,
                    'category': (";".join([item['category'] for item in summary.transactions])
                                 if combine_categories
                                 else summary.transactions[0]['category']),
                    'combicount': ";".join([itemized_amount.compact_row_string(item) for item in summary.transactions]) if debug else len(summary.transactions),
                    'item': (";".join([(item.get('item') or "") for item in summary.transactions])
                               if combine_categories
                               else (summary.transactions[0].get('item') or "")),
                    'timestamp': adjusted_datetime,
                    'account': sumkey[0],
                    'payee': sumkey[1],
                    'unique_number': unique_number,
                    'date': adjusted_datetime.date().isoformat(),
                    'time': adjusted_datetime.time().isoformat()}
                unique_number += 1
        if False:
            print("results:")
            for timestamp in sorted(result.rows):
                # print("cspe res  ", timestamp, result.rows[timestamp], itemized_amount.row_descr(result.rows[timestamp]))
                print("cspe res  ", timestamp, repr(result.rows[timestamp]['amount']))
        return result

    def count_same_period_categories(self, period):
        """Produce a sheet based on this one, with a time column and columns counting each category in that time period.

        `period' is a function which should return the starting
        datetime.datetime of the period containing the date it is
        given.

        """
        rows = {}
        column_names = []
        for timestamp in self.rows.keys():

            row = self.rows[timestamp]
            row_date = period(timestamp)
            if row_date not in rows:
                rows[row_date] = {'timestamp': row_date}
            this_period_by_category = rows[row_date]

            category = row.get('category', 'unknown')
            if category not in column_names:
                column_names.append(category)

            this_period_by_category[category] = this_period_by_category.get(category, 0) + 1

        return named_column_sheet.named_column_sheet(self.config, ['timestamp'] + sorted(column_names), rows)

    def construct_row(self,
                      timestamp,
                      amount=0,
                      currency='GBP',
                      payee=None,
                      category=None):
        timestamp = self.unused_timestamp_from(timestamp)
        self.rows[timestamp] = {'timestamp': timestamp,
                                'date': timestamp.date().isoformat(),
                                'amount': amount,
                                'currency': currency,
                                'payee': payee,
                                'category': category}

    def find_nearest_after(self, timestamp):
        """Return the row nearest after a timestamp."""
        for ts in sorted(self.rows.keys()):
            if ts >= timestamp:
                return self.rows[ts]
        return None

    def adjustments(self, other, account_name, period):
        """Return a sheet containing adjustments need to align a sheet with another.
        Existing adjustments are ignored.  The balance is tracked here, and any existing
        balance in this sheet is ignored (but the balances from the other sheet are used)."""
        result = canonical_sheet(self.config)
        balance = 0
        period_start = None
        for timestamp in sorted(self.rows):
            row = self.rows[timestamp]
            if row.get('account') != account_name:
                continue
            if row.get('category') != 'adjustment':
                balance += row.get('amount', 0)
            this_period = period(timestamp)
            if this_period != period_start:
                statement_row = other.find_nearest_after(this_period)
                if not statement_row:
                    break           # no more statements
                statement_balance = float(statement_row.get('balance', 0))
                imbalance = statement_balance - balance
                print("imbalance at", period_start, "is", imbalance)
                result.construct_row(this_period, imbalance, category='adjustment')
                period_start = this_period
                balance = statement_balance
        return result

    def occupied_columns(self):
        """Return a sheet like this but with only the columns that are in use."""
        return named_column_sheet.named_column_sheet.occupied_columns(self)

    def write_csv(self, filename, suppress_timestamp=True):
        """Write a canonical spreadsheet to a file.
        Any columns not in the canonical format are ignored."""
        full_filename = os.path.expanduser(os.path.expandvars(filename))
        # print("writing canonical csv", full_filename, "from", self)
        qsutils.ensure_directory_for_file(full_filename)
        column_sequence = (canonical_sheet.canonical_column_sequence_no_timestamp
                           if suppress_timestamp
                           else canonical_sheet.canonical_column_sequence)
        with open(full_filename, 'w') as outfile:
            writer = csv.DictWriter(outfile, column_sequence)
            writer.writeheader()
            for timestamp in sorted(self.rows.keys()):
                row = self.rows[timestamp]
                if (row.get('original_currency') == row.get('currency')
                    and row.get('original_amount') == row.get('amount')):
                    row['original_currency'] = None
                    row['original_amount'] = None
                if row.get('date', "") == "":
                    row['date'] = timestamp.date()
                if row.get('time', "") == "":
                    row['time'] = timestamp.time()
                # select only the columns required for this sheet, and
                # also round the unfortunately-represented floats
                output_row = {sk: qsutils.tidy_for_output(row.get(sk, ""))
                              for sk in column_sequence}
                writer.writerow(output_row)

    def write_debug(self, filename):
        """Write a account to a file, for debugging."""
        with open(os.path.expanduser(os.path.expandvars(filename)), 'w') as outfile:
            outfile.write(str(self))

# tests

import argparse

def main():
    """Tests for this module."""
    parser = qsutils.program_argparser()
    parser.add_argument("input_files", nargs='*')
    args = parser.parse_args()
    config = qsutils.program_load_config(args)
    for filename in args.input_files:
        for all_rows in (False, True):
            print("reading and converting", filename)
            sheet = canonical_sheet(config, input_sheet=filename, convert_all=all_rows)
            print("canonical sheet from", filename, "is", sheet)
            print("---- begin sample of", len(sheet), "all" if all_rows else "filtered", "canonical rows ----")
            countdown = 16
            for row in sheet:
                print(row)
                countdown -= 1
                if countdown == 0:
                    break;
            print("---- end canonical rows ----")
            accounts, added_row_lists = sheet.distribute_to_accounts()
            print("account names for", "unfiltered" if all_rows else "filtered", filename, "are", sorted(accounts.keys()), "and added_row_lists are", sorted(added_row_lists.keys()))
            for k in sorted(accounts.keys()):
                print(k, accounts[k])
            for k in sorted(added_row_lists.keys()):
                print(k, added_row_lists[k][:8])

if __name__ == "__main__":
    main()
