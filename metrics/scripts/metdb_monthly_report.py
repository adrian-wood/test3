#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-
# -----------------------------------------------------------------------
#
# PROGRAM       : metdb_monthly_report.py
#
# PURPOSE       : Finds start and end dates of last month and runs acripts
#                 to produce monthly summary web pages.
#
# USAGE         : run manually after downloading pdf files from Nagios.
#
# REVISION INFO :
#
# $Log:

# MB-1743: Moved to mdb-apps server; now using Nagios PDFs.        SN
# MB-1608: Version for Nagios XI graphs, based on code previously held
#          in metdb-apps/metrics.                                  SN
# $
# -----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2013- MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom.
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
# -----------------------------------------------------------------------

import sys
import os
import datetime as dt
import calendar
import traceback

BASEDIR = r"/var/moods/metrics/"
HTMLDIR = r"/var/www/html/metrics/"
ARCHIVEDIR = HTMLDIR + "archive/"


def getLastMonth(today):

    # returns a date object set to the first day of last month

    d = dt.timedelta(days=5)
    sameMonth = True
    thisMonth = today.strftime("%m")

    while sameMonth:

        then = today - d
        lastMonth = then.strftime("%m")
        sameMonth = thisMonth == lastMonth
        today = then
        thisMonth = lastMonth

    return then.replace(day=1)

# -------------------------------------------------------------------------


def main():

    # get dates for first and last days last month.

    today = dt.datetime.today()

    firstOfMonth = getLastMonth(today)
    year = int(firstOfMonth.strftime("%Y"))
    month = int(firstOfMonth.strftime("%m"))
    (first, last) = calendar.monthrange(year, month)
    lastOfMonth = firstOfMonth.replace(day=last)

    startstr = firstOfMonth.strftime("%Y%m%d")
    endstr = lastOfMonth.strftime("%Y%m%d")

    cmd = BASEDIR + "create_summary.py -d " + startstr
    rc = os.system(cmd)
    if rc != 0:
        print "Error running " + cmd
        sys.exit(8)

    # move the summary page to archive and create a 'latest' symlink
    # (no return codes from these functions)
    summary = "monthly_summary_" + startstr + ".html"
    os.rename(HTMLDIR + summary, ARCHIVEDIR + summary)
    os.unlink(HTMLDIR + "latest_report.html")
    os.symlink(ARCHIVEDIR + summary, HTMLDIR + "latest_report.html")

if __name__ == "__main__":
    main()
