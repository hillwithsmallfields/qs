Financial (and other) account handling programs
===============================================

A suite of programs for manipulating, merging, checking, and charting
my self-quantification files: finances and weight, for a start.

The expected workflow is that the programs will take incoming data
files from other sources (typically bank statements), and incorporate
it in central files with conversions as necessary.  The original use
is to import things that appear in my bank statements into my finance
tracking app.

The config file
===============

A lot of the details are driven by a config file, written in YAML.

The default config file location is /usr/local/share/qs-accounts.yaml
and you can add more files on the command line (and suppress the
default one).

The config file describes the spreadsheet formats used by different
banks and apps.  It gives the names used for particular types of
columns.  It should look like this:

    formats:
      handelsbanken:
        column-sequence: ["Date", None,
                          "Details", None,
                          "Money out", None,
                          "Money in", None,
                          "Balance"]
        columns:
          date: Date
          payee: Details
          credits: Money in
          debits: Money out
        currency: "GBP"
        name: "Handelsbanken current account"
        conversions:
          "MYEMPLOYER LTD":
            'category': "Salary"
            'parent': "Incoming"
            'payee': "My Employer Ltd"
          "MY COUNTY COUNCIL":
            'category': "Government"
            'parent': "Routine costs"
            'payee': "Shire Council"
      financisto:
        column-sequence: ["date", "time",
                          "account",
                          "amount", "currency",
                          "original amount", "original currency",
                          "category", "parent",
                          "payee",
                          "location", "project", "note"]
        columns:
          date: date
          payee: payee

Configuration section details
=============================

`formats`
---------

The `formats` section consists of a named section for each account,
describing the format of the spreadsheet and the conversions from that
spreadsheet's data to a standard form.

`formats:column-sequence`
-------------------------

A list of column names exactly as they appear in the spreadsheet, in
order.  Blank columns are represented with `None`.

`formats:columns`
-----------------

A mapping (dictionary) with the keys being a set of column names that
are hard-coded into the programs, and the values being the names used
for those columns in this particular type of spreadsheet.  The names
used in the programs are:

 - `account`
 - `amount`
 - `balance`
 - `category`
 - `credits`
 - `currency`
 - `date`
 - `debits`
 - `location`
 - `message`
 - `original_amount` (for when the currency has been converted)
 - `original_currency` (for when the currency has been converted)
 - `parent` (of the `category`)
 - `payee`
 - `project`
 - `time`

`formats:conversions`
---------------------

A mapping (dictionary) from names that appear in the column described
as `payee` in the input spreadsheet, to mappings describing data to
add to that row in the output spreadsheet.

The keys of these mappings are the same as those in `format:columns`,
as they name the columns to put output into.  The values are fixed
strings to put in those columns.  This is aimed at outputting to
formats such as Financisto, which has a column for categorising items,
with input from formats such as bank statements, which don't have a
category column.  The value for the `payee` column gives the name that
you want to appear in the output, as the names in bank statements are
not necessarily the ones you'd really want to use.

`formats:column-defaults`
-------------------------

Values to put in output columns for which there is no corresponding
input.  For example, if your output format has time and date, and your
input format has only date, you can put a default time value to be put
for all rows imported from that input format.

The programs
============

finconv.py
----------

Program to convert finance spreadsheets between formats with optional
filtering.

Originally written to add the automatic payments reported in my bank
statements to my financisto (Android app) accounts, as I haven't been
in the habit of doing them as they come in.

It uses the `formats:columns` description for the input format to find
certain pieces of data from each input row (such as `amount`, `payee`,
`date`) and constructs an output row using the corresponding column
names from the `formats:columns` description of the output format.

It starts by looking at the header line of each input file, comparing
it with the `formats:column-sequence` descriptions from the config
file, to identify the input format, then reads all the entries in the
input file, converting them as specified and writing them to the
output file.  If the command line option `--update` is used, the file
specified for updating is read before all the others, and then the
result is written back to it.

If the command line option `--all-rows` is given, all rows are
converted; otherwise only the rows for a `payee` mentioned in
`formats:conversions` are processed, except that with `--update` all
the rows of the main file (the one being updated) are processed, and
the filtering applies only to the files being merged into it.

findiscrep.py
-------------

Program to track discrepancies in finance spreadsheets.

It seems to compare a computed balance with an imported balance,
possibly for multiple accounts, using a `comparisons` part of a format
description.

finperiodic.py
--------------

Program to detect periodic payments and spot gaps in them.

Just a stub so far.

finsplit.py
-----------

Program to split an account into a separate column per category or per
payee.

finsum.py
---------

Program to sum the transactions each day (or month, or year) per payee
in finance spreadsheets.

Incomplete.

fintrack.py
-----------

Program to track finance spreadsheets, by keeping our own running
balance from a series of transactions.

prepare_financisto
------------------

Check a file for high-bit-set bytes.

qschart.py
----------

Program to chart my Quantified Self files.

qsmerge.py
----------

Program to merge my Quantified Self files.

qsupdate
--------

Prepare a complete import file to put entries from my bank statements
into Financisto format, so I can filter them and import them.

qsw2t.py
--------

Program to go from my combined body logs to Weight Tracker app format.

If an existing Weight Tracker file is given, any entries in it are
skipped from the output, as the point of the program is to fill in
gaps in the app's data from other data (e.g. from before I started to
use the app).
