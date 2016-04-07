#!/usr/local/sci/bin/python2.7
import sys
import os
import subprocess
import metdbEnv
from datetime import datetime

#====================================================================================    
class TestPlan:
   'Class representing a complete test plan as a dictionary. Key is the test number \
      and value is a test instance'
        
#------------------------------------------------------------------------------------    
   def __init__(self,filename):
      self.tests={}
      self.testCount=0
 
      inp = None
      try:
         inp=open(filename)
         records=inp.readlines()
         inp.close
      except IOError:
         print "IOError opening: " + filename
         os._exit(1)
 
      for r in records:
        cols=r.split(":")
        number = int(cols[0].strip())
        command = cols[1].strip()
	infile = cols[2].strip()
        description=cols[3].strip()
        self.addTest(Test(number,command,description))     
 
#------------------------------------------------------------------------------------    
   def addTest(self,test):
      key=test.getNumber()
      if key in self.tests:
         print "Error - duplicate test number - ",key
         os._exit(1)
      else:
         self.tests[key]=test
         self.testCount += 1
      
#------------------------------------------------------------------------------------    
   def getTest(self,testNumber):
      if testNumber in self.tests:
         return self.tests[testNumber]
 
#------------------------------------------------------------------------------------    
   def getTestCount(self):
      return self.testCount
   
#------------------------------------------------------------------------------------    
   def displayPlan(self):
      print 'TestPlan\n========'
      print '1..%d' % (self.testCount)
      for k,v in self.tests.items():
         v.displayTest()
         
#===================================================================================
class Test:
   'Class representing one test'
#------------------------------------------------------------------------------------    
   def __init__(self, number, command, description):
      self.number = number
      self.command = command
      self.description = description
      self.todo = ''
           
#------------------------------------------------------------------------------------    
   def getNumber(self):
      return self.number
 
#------------------------------------------------------------------------------------    
   def getCommand(self):
      return self.command
    
#------------------------------------------------------------------------------------    
   def getDescription(self):
      return self.description    
 
#------------------------------------------------------------------------------------    
   def getResult(self):
      return self.result
 
#------------------------------------------------------------------------------------    
   def setResult(self,result):
      self.result=result      
 
#------------------------------------------------------------------------------------    
   def getTodo(self):
     return self.todo

#------------------------------------------------------------------------------------    
   def displayTest(self):
      print '%3d : %-20s - %-30s '% (self.number,self.command,self.description)
 
#------------------------------------------------------------------------------------    
   def run(self):
      'Runs the test script, collects the output and checks if it worked or not'

      rc=0
# Need to substitute values for variables on the command line because subprocess does 
# expand them automatically
      commandStr=metdbEnv.subEnv(self.command)
     
      command=commandStr.split(' ')	    
      if "#" in command:
        command.remove('#')
	self.todo=" # TODO "
      print 'command is ', command
      try:
         proc = subprocess.Popen(command,stdout=subprocess.PIPE,
	                            stderr=subprocess.PIPE)
         proc.wait()   # this initialises returncode
         rc=proc.returncode
         print 'command rc',rc
         (stdout,stderr)=proc.communicate()
         print 'stdout=',stdout
         print 'stderr=',stderr
      except OSError:
         print 'Error calling ',command
	 rc = 1

      if rc == 0 :
         return True
      else:
         return False
      
#===================================================================================
class Tap:
   'Test Anything Protocol functions'
   
#------------------------------------------------------------------------------------    
   def __init__(self,filename):
      'Open dataset for writing'
      try:
         self.fo=open(filename,"w+")
      except IOError:
         print "Error opening TAP output -",filename
         os._exit(1)
         
#------------------------------------------------------------------------------------    
   def output(self,str):
      'Write a line of TAP'
      try:
         self.fo.write(str + "\n")
         self.fo.flush()
      except IOError:
         print "Error writing TAP output -",str
         
#------------------------------------------------------------------------------------    
   def resultLine(self,test):
      'Generate TAP result line'
      if test.getResult():
         r = "ok "
      else:
         r = "not ok "
         
      text = str(test.getNumber()) + " " + test.getDescription() + test.getTodo()
      self.output(r + text)
 
#------------------------------------------------------------------------------------    
   def printTap(self):
      print "TAP output ==> ", self.fo.name
      self.fo.seek(0,0)
      tapStream=self.fo.readlines();
      for line in tapStream:
         print line,
      
#------------------------------------------------------------------------------------    
   def close(self):
      self.fo.close()
      self.fo=None
 
#===================================================================================
def setEnv(file):
   envars={}
   try:
      FH = open(file,'r')
      lines = FH.readlines()
      for line in lines:
         if line.find('#') == -1 :
            parts = line.split('=')
	    if len(parts) == 2:
               varName = parts[0]
               varVal = parts[1].strip()
               os.environ[varName] = varVal
	       envars[varName] = varVal
   except:
# Handle errors but do nothing
      pass
   finally:
      if FH: FH.close()

   return envars


#===================================================================================
def main():
 
# set up environment
   metdbEnv.setEnv('SetEnv.txt')

# create and initialise test plan
   TESTPLAN=os.environ.get('TESTPLAN')
   if not TESTPLAN:
     print "No TestPlan found "
     os._exit(1)
   plan = TestPlan(TESTPLAN)     

   plan.displayPlan()
 
# Set up TAP output
   TESTRESULTS=os.environ.get('TESTRESULTS','TestResults.txt')
   tap=Tap(TESTRESULTS)
   tap.output("1.."+ str(plan.testCount))
   now=datetime.utcnow().strftime('%a %d %b @ %H:%M:%S')+'Z'
   tap.output("# START "+now)
 
# Run the testplan
   for n in range(1,plan.testCount+1):
      test=plan.getTest(n)
      try:
         if test:
            test.setResult(test.run())
            tap.resultLine(test)
               
      except FailTest:
         test.setResult(False)
         tap.resultLine(test)
         
   now=datetime.utcnow().strftime('%a %d %b @ %H:%M:%S')+'Z'
   tap.output("# END "+now)
   tap.printTap()   
   tap.close()
   
if __name__ == "__main__":
   main()
 
