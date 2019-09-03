"""
Main program for running MetDB Retrieval regression tests.

The following environment variables must be set before running
this script:

   BASEDIR - /var/moods/regression/

   SYS1 - system being tested e.g. mdbap-preprod
   SYS2 - system for comparison e.g. mdbap-preprod
   VER1 - version being tested e.g. 5.20.0
   VER2 - version for comparison e.g. 5.19.0
   WEB1 - system for testing web retrievals e.g. mdbdb-preprod
   WEB2 - system for web retrieval comparisons e.g. mdbdb-prod

   TESTPLAN - absolute name of Test Plan    
   TESTRESULTS - absolute name for results output
   SCRIPTS - location of runner scripts
   INDIR - location of &INPUT files
   BINDIR - location of retrieval executables

   WEBROOT - root of location for web output
   CGIBIN - location of CGI scripts
   OUTDIR - location for retrieval output
   WEBLINK - link address for archive data
   ARCHIVEDIR - location for archive data


"""
import sys
import os
import TestAnything as ta
import subprocess
from datetime import datetime


def main():

    # create and initialise test plan
    TESTPLAN = os.environ.get('TESTPLAN')
    if not TESTPLAN:
        print("No TestPlan found ")
        sys.exit(1)
    plan = ta.TestPlan()
    plan.readPlan(TESTPLAN)

    print(plan)

    # Set up TAP output
    TESTRESULTS = os.environ.get('TESTRESULTS', 'TestResults.txt')
    plan.startRun(TESTRESULTS)

    # Run the testplan
    for n in range(1, plan.count + 1):
        t = plan.getTest(n)
        if t:
            print(t)
            t.run()
            plan.resultLine(t)


    plan.endRun()


if __name__ == "__main__":
    main()
