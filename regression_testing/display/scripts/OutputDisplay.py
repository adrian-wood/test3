#!/usr/bin/env python
import cgi
import cgitb; cgitb.enable()
import sys
import os,os.path
import difflib
import pygments
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
  metdbEnv.setEnv('SetEnv.txt')
  webRoot=os.environ.get('WEBROOT','.')
  form = cgi.FieldStorage()
  infile=form.getvalue('infile')
  dateDir=form.getvalue('dateDir')
  # only need last part of infile location
  fullpath=metdbEnv.subEnv(infile)
  file=os.path.basename(fullpath)
  sys1=os.environ.get('SYS1')
  sys2=os.environ.get('SYS2')
  outdir=os.environ.get('OUTDIR')
  out1=readData(outdir+'/'+sys1+'/'+dateDir+'/'+file+'.out')
  out2=readData(outdir+'/'+sys2+'/'+dateDir+'/'+file+'.out')
  d=difflib.HtmlDiff()
  out3=d.make_table(out1,out2,fromdesc=sys1,todesc=sys2)
  log1=readInp(outdir+'/'+sys1+'/'+dateDir+'/'+file+'.log')
  log2=readInp(outdir+'/'+sys2+'/'+dateDir+'/'+file+'.log')
  templateVars={"out3" : out3, 
         		"log1" : log1,
		        "log2" : log2,
		        "sys1" : sys1,
	         	"sys2" : sys2} 
  print render(webRoot,viewer,**templateVars)

#--------------------------------------------------------------------------
if __name__ == "__main__":
   main()

