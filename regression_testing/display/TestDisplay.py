#!/usr/bin/env python
import re
import cgi
from datetime import datetime
import cgitb; cgitb.enable()
import sys
import os
import urllib
import metdbEnv
from jinja2 import FileSystemLoader, Environment

def render(directory, template_name, **kwargs):
  loader = FileSystemLoader(directory)
  env = Environment(loader=loader)
  template = env.get_template(template_name)
  output= "Content-Type: text/html;charset=utf-8\n\n" + template.render(**kwargs)
  return output
#===================================================================================
class TAP:
  'Class representing a complete TAP file'

#----------------------------------------------------------------------------------
  def __init__(self,filename):
    self.tests={}
    self.testCount=0   # total number of tests
    self.testPass=0    # number passed
    self.testFail=0    # number failed
    self.testBonus=0   # number of TODO tests passed

    records=readData(filename)
    for row in records:
      if row[0:7] == '# START':
        self.t1=row[8:]
      if row[0:5] == '# END':
        self.t2=row[6:]
      if row.find('..') == -1 or row[0:1] != '#':
        columns = row.split(' ')
        if columns[0].find('not') != -1:
          result='fail'
	  col=2
        elif columns[0].find('ok') != -1:
          result='pass'
	  col=1
        else:
          continue
	testnum=columns[col]
        description=' '.join(columns[col+1:])
        if description.find('# TODO') >= 1:
          todo=True
        else:
	  todo=False
        self.addTest(Test(testnum,result,description,todo))

#----------------------------------------------------------------------------------
  def addTest(self,test):
    key=test.getNumber()
    if key in self.tests:
      err= "Error - duplicate test number - "+ key
      errorExit(err)
    else:
      self.tests[key]=test
      self.testCount += 1
      if test.getResult() == 'pass': 
        self.testPass += 1
	if test.getTodo() :
	  self.testBonus +=1
      else:
        self.testFail += 1

#------------------------------------------------------------------------------------
  def getTest(self,testNumber):
    if testNumber in self.tests:
      return self.tests[testNumber]

#------------------------------------------------------------------------------------
  def getTestCount(self):
    return self.testCount

#------------------------------------------------------------------------------------
  def getT1(self):
    return self.t1

#------------------------------------------------------------------------------------
  def getT2(self):
    return self.t2

#------------------------------------------------------------------------------------
  def getTestPass(self):
    return self.testPass

#------------------------------------------------------------------------------------
  def getTestFail(self):
    return self.testFail

#------------------------------------------------------------------------------------
  def getTestBonus(self):
    return self.testBonus

#===================================================================================
class Test:
  'Class representing one test'
  #------------------------------------------------------------------------------------
  def __init__(self, number, result, description, todo):
    self.number = number
    self.result = result
    self.description = description
    self.todo = todo

#------------------------------------------------------------------------------------
  def getNumber(self):
    return self.number

#------------------------------------------------------------------------------------
  def getResult(self):
    return self.result

#------------------------------------------------------------------------------------
  def getDescription(self):
    return self.description

#------------------------------------------------------------------------------------
  def getTodo(self):
    return self.todo

#------------------------------------------------------------------------------------
  def displayTest(self):
    print '%3d : %-6s %-20s - %-30s '% (self.number,self.todo,self.result,self.description)

#---------------------------------------------------------------------------
def readData(filename):
  inp=None

  inp=open(filename,'r')
  lines=inp.readlines() 
  inp.close()   
  return lines
 
#-------------------------------------------------------------------------
def getResult(testnum,results):
  if testnum in results:
    return results[testnum]
  else:
    return 'fail'

#--------------------------------------------------------------------------
def parseData(table,results):
  rows=[]
  
  for row in table:
    columns=row.split(":")
    if len(columns) > 1:
      testnum=columns[0].strip()
      result=results.getTest(testnum).getResult()
      cmd=columns[1]
      inp=columns[2].strip()
      detail=readInp(inp)
      description=results.getTest(testnum).getDescription()
      rows.append((testnum,cmd,inp,detail,description,result))
  return rows

#--------------------------------------------------------------------------
def readInp(infile):
  fullname=metdbEnv.subEnv(infile)
  content=readData(fullname)
  return ''.join(content)

#--------------------------------------------------------------------------
def main():
#  testPlan=r"/home/h01/ussn/new_integration/scripts/TestPlan.txt"
#  testResults=r"/home/h01/ussn/new_integration/scripts/TestResults.txt"
#  webRoot=r"/home/h01/usmdb/public_html/moods/test_viewers/"
  viewer=r"testplan_template.html"
  error=r"testplan_error.html"
  now=datetime.utcnow().strftime('%a %d %b @ %H:%M:%S')+'Z'

  metdbEnv.setEnv('/home/h01/ussn/new_integration/scripts/SetEnv.txt')
  webRoot=os.environ.get('WEBROOT','.')
  testPlan=os.environ.get('TESTPLAN')
  testResults=os.environ.get('TESTRESULTS')
  try:
    table=readData(testPlan)
  except IOError as err:
    templateVars={"errorMsg" : err }
    print render(webRoot,error,**templateVars)
    sys.exit(1)
  
  try:
    tap=TAP(testResults)
  except IOError as err:
    templateVars={"errorMsg" : err }
    print render(webRoot, error, **templateVars)
    sys.exit(1)

  rows=parseData(table,tap)
  sys1=os.environ.get('SYS1')
  sys2=os.environ.get('SYS2')
  templateVars={"row" : rows, 
                "count" : tap.getTestCount(), 
		"pass" : tap.getTestPass(), 
		"fail" : tap.getTestFail(), 
		"bonus" : tap.getTestBonus(), 
		"t1" : tap.getT1(),
		"t2" : tap.getT2(),
		"sys1" : sys1,
		"sys2" : sys2}
  print render(webRoot,viewer,**templateVars)

#--------------------------------------------------------------------------
if __name__ == "__main__":
   main()

