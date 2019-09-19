'''Produce a summary page of regression tests.

   Submitted by TestRunner.py once all the regression tests have
   completed.  Can be re-run from the command line; no arguments.

   It gets and sets environment variables from the config file
   SetEnv.txt.

   The web page is archived and a symlink updated to point to the
   latest version.
'''

import os
import sys
sys.path.append('/var/moods/regression/scripts/')
import TestAnything as ta
import re
import cgi
from datetime import datetime
import cgitb
sys.path.append('/var/moods/metdb_utils/web')     # local utils
from jinja_render import jinja_render
from daily_archive import daily_archive
cgitb.enable()


# --------------------------------------------------------------------------
def parseData(plan):

    rows = []

    for k, t in sorted(plan.tests.items()):
        print(t)
        testnum = t.number
        if t.result:
            result = 'pass'
        else:
            result = 'fail'
        cmd = t.command
        inp = t.docref.strip()
        detail = readInp(inp)
        description = t.description
        rows.append((testnum, cmd, inp, detail, description, result))
    return rows

def readInp(infile):

    fullname = os.path.expandvars(infile)
    try:
        inp = open(fullname, 'r')
        lines = inp.readlines()
        inp.close()
    except IOError:
        lines = ['No information']
    return ''.join(lines)

# --------------------------------------------------------------------------
def testDisplay():
    viewer = r"testplan_template.html"
    error = r"testplan_error.html"
    dateDir = datetime.utcnow().strftime('%Y/%m/%d')

    webRoot = os.environ.get('WEBROOT', '.')
    archiveDir = os.environ.get('ARCHIVEDIR')
    webLink = os.environ.get('WEBLINK')
    testPlan = os.environ.get('TESTPLAN')
    testResults = os.environ.get('TESTRESULTS')
    # Read the test plan
    plan = ta.TestPlan()
    plan.readPlan(testPlan)
    # read the test results
    plan.readTAP(testResults)
    rows = parseData(plan)
    plan.summarise()

    sys1 = os.environ.get('SYS1')
    sys2 = os.environ.get('SYS2')
    ver1 = os.environ.get('VER1')
    ver2 = os.environ.get('VER2')

    templateVars = {"row": rows,
                    "count": plan.count,
                    "pass": plan.success,
                    "fail": plan.fail,
                    "bonus": plan.bonus,
                    "t1": plan.start_time,
                    "t2": plan.end_time,
                    "sys1": sys1,
                    "sys2": sys2,
                    "ver1": ver1,
                    "ver2": ver2,
                    "dateDir": dateDir,
                    "webLink": webLink}
    htmlout = jinja_render(webRoot, viewer, **templateVars)
    
    daily_archive(webRoot + "/latest_regression_tests.html",
                  archiveDir, htmlout)


# --------------------------------------------------------------------------
if __name__ == "__main__":
    testDisplay()
