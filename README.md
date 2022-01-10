My Quantified Self code
=======================

This badly needs either a re-write or a major refactoring, and I'm
embarrassed about how rambling it has become.  However, I also have
other things to do, and it largely works.  A bit like quite a lot of
closed-source commercial software.

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

For atavistic reasons, partway through this I decided the upper levels
of it would be better in Lisp, so I wrote a small Lisp interpreter for
it.  However, the Lisp parts of the code are very small, and I'm not
sure it was a good idea, although it does nicely separate the upper
and lower parts of the code.

This is probably the part that needs most rewriting.
