Financial (and other) account handling programs
===============================================

A suite of programs for manipulating, merging, checking, and charting
my self-quantification files: finances and weight, for a start.

A lot of the details are driven by a config file, written in YAML.

The default config file location is /usr/local/share/qs-accounts.yaml
and you can add more files on the command line (and suppress the
default one).

The config file describes the spreadsheet formats used by different
banks and apps.  It gives the names used for particular types of
columns.  It should look like this:

    formats:
      handelsbanken:
        column-sequence: ["Date", None, "Details", None, "Money out", None, "Money in", None, "Balance"]
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
        column-sequence: ["date", "time", "account", "amount", "currency", "original amount", "original currency", "category", "parent", "payee", "location", "project", "note"]
        columns:
          date: date
          payee: payee

finconv.py
----------

Program to filter finance spreadsheets and convert them between
formats.

Originally written to add the automatic payments reported in my bank
statements to my financisto (Android app) accounts, as I haven't been
in the habit of doing them as they come in.

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
