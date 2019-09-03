"""
TestAnything module provides functions for MetDB retrieval regression tests.

Two classes are Test which describes a single regression test and TestPlan
which is a collection of all the tests used in a run.

As a TAP producer this provides functions to read the TestPlan, execute the
tests as Unix sub-processes, gather the results and output them in TAP format.
For example:

   >>> import TestAnything as ta
   >>> plan = ta.TestPlan()               # initialise a test plan
   >>> plan.readPlan('TestPlan.txt')      # read in the tests
   >>> plan.startRun('TestResults.txt')   # initialise the output file
   >>> for n in range(1, plan.count + 1): # loop over tests in the plan
   ...     t = plan.getTest(n)            # get a test
   ...     if t:
   ...         t.run()                    # run it
   ...         plan.resultLine(t)         # record the result
   >>> plan.endRun()                      # end the run

Although TAP output is human readable, it does not contain all the information
in the test plan.  As a TAP consumer there are functions to read a file of TAP
results, to combine with the extra information in the test plan to output a
useful display.

   >>> import TestAnything as ta
   >>> plan = ta.TestPlan()
   >>> plan.readPlan('TestPlan.txt')
   >>> plan.readTAP('TestResults.txt')
   >>> plan.summarise()
   >>> print(plan.success, plan.fail, plan.bonus)
   >>> for k, t in sorted(plan.tests.items()):
   ...     print(t.number, t.description, t.command, t.result)


"""
import sys
import os
import subprocess
from datetime import datetime

TIMEOUT = 180   # time limit for subprocess


class TestPlan:
    """Class representing a complete test plan. For each test,
       Key is the test number and value is a test instance
    """

    def __init__(self):
        """Initialise an empty test plan instance"""
        self.tests = {}
        self.count = 0   # number of tests
        self.success = 0    # number passed
        self.fail = 0    # number failed
        self.bonus = 0   # number of TODO tests passed
        self.start_time = None  # start of run
        self.end_time = None    # end of run

    def summarise(self):
        """count successes, failures and bonuses"""
        self.success = 0
        self.fail = 0
        self.bonus = 0

        for n in range(1, self.count + 1):
            if self.tests[n].result:
                self.success += 1
                if self.tests[n].todo:
                    self.bonus += 1
            else:
                self.fail += 1

    def readPlan(self, filename):
        """Read a test plan from the given file to populate the
           TestPlan instance."""

        inp = None
        try:
            inp = open(filename)
            records = inp.readlines()
            inp.close()
        except IOError:
            print('Error reading Test Plan: ' + filename)
            sys.exit(1)

        for row in records:
            cols = row.split(":")
            if len(cols) != 4:
                print('Expected four columns in row:', row.strip())
                sys.exit(1)
            try:
                number = int(cols[0].strip())
            except ValueError:
                print('Invalid number format in row:', row.strip())
                sys.exit(1)

            command = cols[1].strip()

            if '#' in command:
                todo = True
            else:
                todo = False

            docref = cols[2].strip()
            description = cols[3].strip()
            self.addTest(Test(number, command, docref,
                              description, todo, None))

    def addTest(self, test):
        """Add or replace a test in the test plan"""

        key = test.number
        if key in self.tests:
            self.tests[key] = test
        else:
            self.tests[key] = test
            self.count += 1

    def getTest(self, number):
        """Get the given Test number"""

        if number in self.tests:
            return self.tests[number]
        else:
            return None

    def readTAP(self, filename):
        """Read test results from the given file to update the
           TestPlan instance."""

        inp = None
        try:
            inp = open(filename, 'r')
            records = inp.readlines()
            inp.close()
        except IOError:
            print('Error reading Test Results: ' + filename)
            sys.exit(1)
        test_count = 0
        for row in records:
            if row[0:7] == '# START':
                self.start_time = row[8:]

            if row[0:5] == '# END':
                self.end_time = row[6:]

            if row.find('..') == -1 and row[0:1] != '#':
                columns = row.split(' ')
                todo = columns[-1] == 'TODO'
                if columns[0].find('not') != -1:
                    result = False
                    col = 2
                elif columns[0].find('ok') != -1:
                    result = True
                    col = 1
                else:
                    print('Invalid test result ', row)
                    sys.exit(1)

                testnum = int(columns[col])
                test_details = self.getTest(testnum)
                if test_details:
                    test_details.result = result
                    test_count += 1
                else:
                    print('Test number ', testnum, 'not in plan')
                    sys.exit(1)

        if test_count != self.count:
            print('Test Results do not match Test Plan')
            sys.exit(1)

    def startRun(self, filename):
        """Open dataset for writing"""

        try:
            self.fo = open(filename, "w+")
        except IOError:
            print('Error opening TAP output -', filename)
            sys.exit(1)

        self.output('1..' + str(self.count))
        now = datetime.utcnow().strftime('%a %d %b @ %H:%M:%S') + 'Z'
        self.output('# START ' + now)

    def endRun(self):
        """Add the end time and close the output dataset"""

        now = datetime.utcnow().strftime('%a %d %b @ %H:%M:%S') + 'Z'
        self.output('# END ' + now)
        self.fo.close()

    def output(self, str):
        """Write a line of TAP"""

        try:
            self.fo.write(str + "\n")
            self.fo.flush()
        except IOError:
            print('Error writing TAP output -', str)

    def resultLine(self, t):
        """Generate TAP result line"""

        if t.result:
            r = 'ok '
        else:
            r = 'not ok '

        if t.todo:
            todo = ' # TO DO '
        else:
            todo = ''
        text = str(t.number) + ' ' + t.description + \
            todo
        self.output(r + text)

    def __repr__(self):
        """String representation of a test plan"""

        text = ('TestPlan\n========\n')
        if self.count > 0:
            text += '1..%d\n' % (self.count)
            for k, v in list(self.tests.items()):
                text += str(v)
        else:
            text += '(empty)'

        return text


class Test:
    """Class representing one test"""

    def __init__(self, number, command, docref, description, todo, result):
        self.number = number
        self.command = command
        self.docref = docref
        self.description = description
        self.todo = todo
        self.result = result      # True for pass, otherwise False

    def __repr__(self):
        """String representation of a test"""

        line = '%3d : %-20s - %-30s \n' % \
               (self.number, self.command, self.description)
        return line

    def run(self):

        """Run the test script, collect the output and check if it worked
         or not"""

        rc = 0
        # Need to substitute values for variables on the command line because
        # subprocess does not expand them automatically
        commandStr = os.path.expandvars(self.command)

        command = commandStr.split(' ')
        if "#" in command:
            command.remove('#')
        print('command is ', command)
        try:
            proc = subprocess.run(command, stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE,
                                  timeout=TIMEOUT)
            stdout = proc.stdout.decode()
            stderr = proc.stderr.decode()
            rc = proc.returncode
            print('command rc', rc)
            print('stdout=', stdout)
            print('stderr=', stderr)
        except (OSError, subprocess.TimeoutExpired) as e:
            print('Error calling ', command)
            print(e)
            rc = 1

        if rc == 0:
            self.result = True
        else:
            self.result = False
