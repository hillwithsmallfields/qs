My Quantified Self code
=======================

Weight and fitness
==================

I collect up data via various apps (and have used different ones in
the past); and sometimes directly into spreadsheets.  I've done some
graphing of them in the past.  Now I'm going to try to bring the lot
together, with "qsmerge" and "qschart" programs.

I'll try them in Python first, for convenience.  As I get older, and
more data accumulates, machines will be getter larger and faster.  A
poor excuse, but I'll stick with it for now.

qsmerge
-------

This accumulates data from multiple CSV files, converting their rows
into a common set of columns (assuming the files being read are for
the same type of data).

qschart
-------

Stub only, for now.  Will pick columns, set up labelling, etc, then
pass the data to gnuplot.

qsw2t
-----

This converts my weight file format into the format used by the Weight
Tracker app on my phone (Android).

Finances
========

finconv
-------

For finance data, I've been quite good at accumulating expenditure
that I do manually, using "Financisto" on my phone, but I've neglected
to add Direct Debits etc.  I started the program "finconv" to filter
and convert the data from my bank statements (it wasn't quite in the
scope of the csvutils I found); then I added features to it to let it
make an overall file within which the comparisons can be done.

fintrack
--------

"fintrack" is for tracking transactions, updating the running balance,
so that I can compare them with the balances provided in the bank
statements.  Easy enough to do in a spreadsheet, but that doesn't
really fit into a scripted workflow, as data and formula would need
sticking together.

finsum
------

"finsum" combines all the transactions for a payee, or for a category,
in a day, month, or year.  It's fiddling around towards solutions to
the problem that things can appear in my bank statement on a different
date from the one they nominally happen.
