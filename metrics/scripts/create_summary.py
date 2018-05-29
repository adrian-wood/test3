#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-
# -----------------------------------------------------------------------
#
# PROGRAM       : create_summary.py
#
# PURPOSE       : Using a template file this overrides all occurrences of
#                 %YYYYMMDD% with the date supplied to create a complete
#                 html document.
#
# USAGE         : see readcommand() below
#                 or run create_summary.py -h
#
# REVISION INFO :
#
#
# $Log:
# MB-1743: Moved to mdb-apps.                                      SN
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
import re
from optparse import OptionParser
import sys
import os
import traceback
import datetime as dt
import jinja2

# -------------------------------------------------------------------------------


def readcommand():
    description = "Description: creates monthly summary html."
    parser = OptionParser(description=description)

    parser.add_option("-b", dest="base",
                      default="/var/www/html/metrics/",
                      help="Base directory.")

    parser.add_option("-t", dest="template",
                      default="/summary_template.html",
                      help="Template HTML file relative to BASE dir.")

    parser.add_option("-d", dest="date",
                      help="date of PDF files yyyymmdd.")

    parser.add_option("-o", "--outfile", dest="outfile",
                      default="/monthly_summary_%YYYYMMDD%.html",
                      help="output file relative to BASE dir.")

    (options, args) = parser.parse_args()
    if not options.date:
        parser.error("date option required")
        sys.exit(8)

    return (options.base, options.template, options.date, options.outfile)

# -------------------------------------------------------------------------------


def writeHtml(base, template, templateVars, outfile):

    try:
        templateLoader = jinja2.FileSystemLoader(searchpath=base)
        templateEnv = jinja2.Environment(loader=templateLoader)
        template = templateEnv.get_template(template)
        output = template.render(templateVars)
    except:
        print "Jinja2 error rendering HTML"
        traceback.print_exc()
        sys.exit(8)

    out = None
    try:
        out = open(base+outfile, 'w')
        out.write(output)
        out.close()
    except IOError:
        print "Error writing file: " + outfile
        sys.exit(8)

# -------------------------------------------------------------------------------


def main():

    # get command line arguments
    (base, template, date, outfile) = readcommand()

    # Get date substitution for output filebame
    pattern = re.compile(r"%YYYYMMDD%")
    outfile = pattern.sub(date, outfile)

    # get the date & time now
    today = dt.datetime.today()
    updatedAt = today.strftime("%a %b %d %H:%M:%S %Y")

    # ... and report month & year for title
    reportDate = dt.datetime.strptime(date, "%Y%m%d")
    reportMonth = reportDate.strftime("%B %Y")

    # Check the report files exist
    sla = "/archive/" + date + "_sla.pdf"
    if not os.path.isfile(base+sla):
        print "Error missing file: " + sla
        sys.exit(8)

    graph = "/archive/" + date + "_graphs.pdf"
    if not os.path.isfile(base+graph):
        print "Error missing file: " + graph
        sys.exit(8)

    webBase = "http://mdb-apps/metrics/"
    sla_report = webBase + sla
    graph_report = webBase + graph

    # set up a dictionary to map html variables to actual values.
    # variables in the HTML appear between {{  }}
    templateDict = {"sla": sla_report,
                    "graphs": graph_report,
                    "report_month": reportMonth,
                    "creator": sys.argv[0],
                    "runner": os.getenv('USER'),
                    "where": os.uname()[1]}
    # create HTML
    writeHtml(base, template, templateDict, outfile)

if __name__ == "__main__":
    main()
