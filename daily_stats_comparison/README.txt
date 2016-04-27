Compare Daily Stats from Pre and Prod, and Produce Webpage
==========================================================

This program reads the daily stats summary pages from both Pre and Prod,
creates a dictionary of subtype/value for each, processes the two
dictionaries and produces a web page of the results.

It creates an archive of these pages - each time it runs, the resulting page
is placed in moods/misc/compare_stats_archive/<yyyy>/<mm>/<dd>, and the symlink
moods/misc/new_compare_stats.html is re-created to link to the most recent page.

The resulting webapge uses Bootstrap CSS for prettifying. No JS or anything else.

The script should be run via cron on www-metdb as user usmdb.
