""" Module to run a single regression test from the test plan.

Assumes the environment has already been set - see wrapper
script for details.

Expects an option -t followed by the test number from the default
TestPlan which is given by environment variable TESTPLAN.

Output from the regression test is written to a subdirectory of
/tmp/TestOne using the usual yyyy/mm/dd format.


"""
import sys
import os
import getopt
import TestAnything as ta

def getargs(argv):
    """Process command line arguments"""

    try:
        opts, args = getopt.getopt(argv, "t:")
    except getopt.GetoptError as err:
        print(str(err))  # will print something like "option -a not recognized"
        sys.exit(1)

    testNo = None
    for o, a in opts:
        if o == "-t":
            testNo = a
        else:
            print("unhandled option ", o)

    if not testNo:
        print("Required argument -t <test_number>")
        sys.exit(1)
    return testNo


def main(argv):
    """Run a single test from the TestPlan"""

    testNo = getargs(argv)

# set OUTDIR so output will be displayed on-screen
    os.environ['OUTDIR'] = '/tmp/TestOne/'

# create and initialise test plan
    TESTPLAN = os.environ.get('TESTPLAN')
    if not TESTPLAN:
        print("No TestPlan found ")
        sys.exit(1)
    plan = ta.TestPlan()
    plan.readPlan(TESTPLAN)

# Set up TAP output
    TESTRESULTS = '/tmp/TestOne/dummy.txt'
    plan.startRun(TESTRESULTS)

    print("Running test ", testNo)
    test = plan.getTest(int(testNo))
    if test:
        test.run()
        plan.resultLine(test)
    else:
        print(f"Test {testNo} not available in {TESTPLAN}")
        os.remove(TESTRESULTS)
        sys.exit(1)

    plan.endRun()

    print("TAP RESULTS\n===========")
    
    f = open(TESTRESULTS, 'r')
    print(f.read())
    f.close()
    os.remove(TESTRESULTS)


if __name__ == "__main__":
    main(sys.argv[1:])
