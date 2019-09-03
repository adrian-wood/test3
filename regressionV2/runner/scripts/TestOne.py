import sys
import os
import getopt
import subprocess
import metdbEnv
from datetime import datetime
sys.path.append('/var/www/cgi-bin/regression/')
from TestDisplay import testDisplay


# =================================================================================
class TestPlan:
    '''Class representing a complete test plan as a dictionary. Key is the
       test number and value is a test instance'''

# ---------------------------------------------------------------------------------
    def __init__(self, filename):
        self.tests = {}
        self.testCount = 0

        inp = None
        try:
            inp = open(filename)
            records = inp.readlines()
            inp.close
        except IOError:
            print("IOError opening: " + filename)
            os._exit(1)

        for r in records:
            cols = r.split(":")
            number = int(cols[0].strip())
            command = cols[1].strip()
            infile = cols[2].strip()
            description = cols[3].strip()
            self.addTest(Test(number, command, description))

# -----------------------------------------------------------------------------------

    def addTest(self, test):

        key = test.getNumber()
        if key in self.tests:
            print("Error - duplicate test number - ", key)
            os._exit(1)
        else:
            self.tests[key] = test
            self.testCount += 1

# -----------------------------------------------------------------------------------

    def getTest(self, testNumber):

        if testNumber in self.tests:
            return self.tests[testNumber]

# -----------------------------------------------------------------------------------

    def getTestCount(self):

        return self.testCount

# -----------------------------------------------------------------------------------

    def displayPlan(self):

        print('TestPlan\n========')
        print('1..%d' % (self.testCount))
        for k, v in list(self.tests.items()):
            v.displayTest()


# ==================================================================================
class Test:
    '''Class representing one test'''

    def __init__(self, number, command, description):

        self.number = number
        self.command = command
        self.description = description
        self.todo = ''

# ---------------------------------------------------------------------------------

    def getNumber(self):

        return self.number

# ----------------------------------------------------------------------------------

    def getCommand(self):

        return self.command

# ---------------------------------------------------------------------------------

    def getDescription(self):

        return self.description

# ----------------------------------------------------------------------------------

    def getResult(self):

        return self.result

# ----------------------------------------------------------------------------------

    def setResult(self, result):

        self.result = result

# -----------------------------------------------------------------------------------

    def getTodo(self):

        return self.todo

# -----------------------------------------------------------------------------------

    def displayTest(self):

        print('%3d : %-20s - %-30s ' %
              (self.number, self.command, self.description))

# ----------------------------------------------------------------------------------

    def run(self):

        '''Runs the test script, collects the output and checks if
           it worked or not'''

        rc = 0
        # Need to substitute values for variables on the command line because
        # subprocess does not expand them automatically
        commandStr = metdbEnv.subEnv(self.command)

        command = commandStr.split(' ')
        if "#" in command:
            command.remove('#')
            self.todo = " # TODO "
        print('command is ', command)
        try:
            proc = subprocess.Popen(command, stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            proc.wait()   # this initialises returncode
            rc = proc.returncode
            print('command rc', rc)
            (stdout, stderr) = proc.communicate()
            print('stdout=', stdout)
            print('stderr=', stderr)
        except OSError:
            print('Error calling ', command)
            rc = 1

        if rc == 0:
            return True
        else:
            return False


# ===============================================================================
class Tap:
    '''Test Anything Protocol functions'''

# -------------------------------------------------------------------------------

    def __init__(self, filename):

        '''Open dataset for writing'''
        try:
            self.fo = open(filename, "w+")
            self.filename = filename
        except IOError:
            print("Error opening TAP output -", filename)
            os._exit(1)

# -------------------------------------------------------------------------------

    def output(self, str):

        '''Write a line of TAP'''
        try:
            self.fo.write(str + "\n")
            self.fo.flush()
        except IOError:
            print("Error writing TAP output -", str)

# ------------------------------------------------------------------------------

    def resultLine(self, test):

        'Generate TAP result line'
        if test.getResult():
            r = "ok "
        else:
            r = "not ok "

        text = str(test.getNumber()) + " " + test.getDescription() +\
            test.getTodo()
        self.output(r + text)

# ------------------------------------------------------------------------------

    def printTap(self):

        print("TAP output ==> ", self.fo.name)
        self.fo.seek(0, 0)
        tapStream = self.fo.readlines()
        for line in tapStream:
            print(line, end=' ')

# ------------------------------------------------------------------------------

    def close(self):

        self.fo.close()
        self.fo = None

# ------------------------------------------------------------------------------

    def delete(self):

        os.remove(self.filename)

# ------------------------------------------------------------------------------


def getargs(argv):

    try:
        opts, args = getopt.getopt(argv, "e:t:")
    except getopt.GetoptError as err:
        # print help information and exit:
        print(str(err))  # will print something like "option -a not recognized"
        os._exit(1)

    envFile = None
    testNo = None
    for o, a in opts:
        if o == "-e":
            envFile = a
        elif o == "-t":
            testNo = a
        else:
            print("unhandled option ", o)

    if not testNo:
        print("Required argument -t <test_number>")
        os._exit(2)
    return (envFile, testNo)

# =============================================================================


def main(argv):
    '''Get command line arguments'''

    (envFile, testNo) = getargs(argv)
    if not envFile:
        envFile = 'SetEnv.txt'

# set up environment
    metdbEnv.setEnv(envFile)

# create and initialise test plan
    TESTPLAN = os.environ.get('TESTPLAN')
    if not TESTPLAN:
        print("No TestPlan found ")
        os._exit(1)
    plan = TestPlan(TESTPLAN)

# Set up TAP output
    TESTRESULTS = 'dummy.txt'
    tap = Tap(TESTRESULTS)
    tap.output("1..1")
    now = datetime.utcnow().strftime('%a %d %b @ %H:%M:%S') + 'Z'
    tap.output("# START "+now)

# Run the testplan
    print("Running test ", testNo)
    test = plan.getTest(int(testNo))
    try:
        if test:
            test.displayTest()
            test.setResult(test.run())
            tap.resultLine(test)

    except FailTest:
        test.setResult(False)
        tap.resultLine(test)

    now = datetime.utcnow().strftime('%a %d %b @ %H:%M:%S') + 'Z'
    tap.output("# END "+now)
    tap.printTap()
    tap.close()
    tap.delete()


if __name__ == "__main__":
    main(sys.argv[1:])
