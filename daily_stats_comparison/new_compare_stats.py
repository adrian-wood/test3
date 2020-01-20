"""Produce a page of stat differences between Pre and Prod.

    (c) Crown copyright 2016, the Met Office.
   From daily stats on prod and preprod this program takes the
   left-hand frames and analyses the stats, producing a page
   of differences etc. A summary is at the top for pasting
   into the Daily Checks Log.
"""

import re
import sys
import os
import urllib.request
import urllib.parse
import urllib.error
import datetime as dt
sys.path.append('/var/moods/metdb_utils/web')
from jinja_render import jinja_render
from daily_archive import daily_archive


def readdata(filename):
    """Read a daily stats summary page and return a dictionary
       keyed on datatype with the number of obs for that datatype.
    """

    stats = {}
    try:
        inp = urllib.request.urlopen(filename)

    except IOError:
        print("Error opening file "+filename)
        sys.exit(1)

    for line in inp:
        line = line.decode('UTF-8')
        if line.startswith("<TD>"):    # it's a datatype line - we want it
            # find the datatype name
            dtp = re.search('html#(.+?)" target', line)
            if dtp:
                dataType = dtp.group(1)

            # find the number of obs - they are either in BLUE or bold RED
            val = re.search('BLUE>(.+?)<', line)
            if val:
                nobs = val.group(1)
            else:
                val = re.search('RED><B>(.+?)<', line)
                if val:
                    nobs = val.group(1)

            # add it to the dictionary
            stats[dataType] = int(nobs)

    return stats


def main():
    """Main program"""

    prodURL = r"http://mdbdb-prod/moods/new_stats/"
    prepURL = r"http://mdbdb-preprod/moods/new_stats/"
    archiveBase = r"/var/www/html/compare_stats_archive"
    archiveUrl = r"/compare_stats_archive/"

    summary = "output1.html"
    full = "mdb_Daily_stats.html"

    # Do some date manipulation to get today's and yesterday's date, used for
    # the output file name and for the link to yesterday's page
    now = dt.datetime.now()
    datestr = now.strftime("%H:%M %d-%m-%Y")
    outDir = archiveBase + now.strftime("/%Y/%m/")
    if not os.path.exists(outDir):
        os.makedirs(outDir)
    outFile = outDir + now.strftime("%d") + ".html"

    yesterday = now - dt.timedelta(days=1)
    yesterdaysPage = ''.join(["/compare_stats_archive/",
                              yesterday.strftime("%Y/%m/%d"),
                              ".html"])

    template_name = "stats_comparison_template.html"
    preValues = readdata(prepURL+summary)
    prodValues = readdata(prodURL+summary)

    # Create a list of unique data types from both dictionaries
    allTypes = sorted(set(list(preValues.keys()) + (list(prodValues.keys()))))

    moreOnPre = []
    moreOnProd = []
    missingOnPre = []
    missingOnProd = []
    detailRows = []

    for dataType in allTypes:
        str1 = dataType
        str4 = 'normal'  # default Booststrap context
        if dataType in prodValues:
            str2 = str(prodValues[dataType])
            if dataType in preValues:
                if prodValues[dataType] == preValues[dataType]:
                    str3 = 'Same'
                elif prodValues[dataType] < preValues[dataType]:
                    str3 = str(preValues[dataType])
                    str4 = 'warning'
                    diff = preValues[dataType] - prodValues[dataType]
                    if prodValues[dataType] == 0:
                        pct = 0.0
                        moreOnPre.append(dataType
                                         + ' (' + str(diff)
                                         + ', '
                                         + 'No data on prod)')
                    else:
                        pct = (float(diff)/prodValues[dataType])
                        moreOnPre.append(dataType
                                         + ' ('
                                         + str(diff)
                                         + ', '
                                         + "{0:.2f}".format((pct)*100)
                                         + '%)')
                else:
                    str3 = str(preValues[dataType])
                    str4 = 'warning'
                    diff = prodValues[dataType] - preValues[dataType]
                    pct = (float(diff)/prodValues[dataType])
                    moreOnProd.append(dataType
                                      + ' ('
                                      + str(diff)
                                      + ', '
                                      + "{0:.2f}".format((pct)*100)
                                      + '%)')
            else:
                missingOnPre.append(dataType)
                str3 = 'Missing'
                str4 = 'info'
            if prodValues[dataType] == 0:
                str4 = 'danger'
        else:
            missingOnProd.append(dataType)
            str2 = 'Missing - new?'
            str3 = str(preValues[dataType])
            str4 = 'info'

        detailRow = (str1, str2, str3, str4)
        detailRows.append(detailRow)

    # Create the keyword:arguments dictionary for the Jinja template
    templateVars = {
        "runtime": datestr,
        "prod_stats_page": prodURL + full,
        "pre_stats_page": prepURL + full,
        "yesterdays_page": yesterdaysPage,
        "archive_url": archiveUrl,
        "moreOnPre": str.join('; ', (moreOnPre)),
        "moreOnProd": str.join('; ', (moreOnProd)),
        "missingOnPre": str.join('; ', (missingOnPre)),
        "missingOnProd": str.join('; ', (missingOnProd)),
        "detailRows": detailRows,
        "creator": sys.argv[0],
        "runner": os.getenv('USER'),
        "where": os.uname()[1]
        }

    # Render the HTML from template, create a page and archive it
    htmlout = jinja_render('/var/moods/daily_stats_comparison', template_name,
                           **templateVars)
    daily_archive("/var/www/html/new_compare_stats.html", archiveBase, htmlout)


if __name__ == "__main__":
    main()
