#!/usr/bin/env python
import cgi
import cgitb; cgitb.enable()
import sys
import os,os.path
import metdbEnv
from jinja2 import FileSystemLoader, Environment

def render(directory, template_name, **kwargs):
  loader = FileSystemLoader(directory)
  env = Environment(loader=loader)
  template = env.get_template(template_name)
  output= "Content-Type: text/html;charset=utf-8\n\n" + template.render(**kwargs)
  return output

#---------------------------------------------------------------------------
def readData(filename):
  inp=None
  inp=open(filename,'r')
  lines=inp.readlines() 
  inp.close()   
  return lines

#--------------------------------------------------------------------------
def readInp(infile):
  content=readData(infile)
  return ''.join(content)

#--------------------------------------------------------------------------
def main():
  viewer=r"output_template.html"
  error=r"testplan_error.html"
  metdbEnv.setEnv('/home/h01/ussn/new_integration/scripts/SetEnv.txt')
  webRoot=os.environ.get('WEBROOT','.')
  form = cgi.FieldStorage()
  infile=form.getvalue('infile')
  # only need last part of infile location
  fullpath=metdbEnv.subEnv(infile)
  file=os.path.basename(fullpath)
  sys1=os.environ.get('SYS1')
  sys2=os.environ.get('SYS2')
  outdir=os.environ.get('OUTDIR')
  out1=readInp(outdir+'/'+sys1+'/'+file+'.out')
  out2=readInp(outdir+'/'+sys2+'/'+file+'.out')
  log1=readInp(outdir+'/'+sys1+'/'+file+'.log')
  log2=readInp(outdir+'/'+sys2+'/'+file+'.log')
  templateVars={"out1" : out1, 
                "out2" : out2,
         		"log1" : log1,
		        "log2" : log2,
		        "sys1" : sys1,
	         	"sys2" : sys2} 
  print render(webRoot,viewer,**templateVars)

#--------------------------------------------------------------------------
if __name__ == "__main__":
   main()

