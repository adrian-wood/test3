#!/usr/bin/env python
'''Produce a web page comparing regression test output and log
   files.  Script is invoked by a link from the main regression
   test summary page. e.g.
     OutputDisplay.py?infile=$INDIR/OPENROAD&dateDir=2018/05/15

'''
import cgi
import cgitb
import sys
import os
import os.path
import difflib
sys.path.append('/var/moods/metdb_utils/web')
from jinja_render import jinja_render
cgitb.enable()
sys.stderr = sys.stdout


# ---------------------------------------------------------------------------
def readData(filename):
    try:
        inp = open(filename, 'r')
        lines = inp.readlines()
        inp.close()
    except IOError:
        lines = ['IOError reading ' + filename]
    return lines


# --------------------------------------------------------------------------
def readInp(infile):
    content = readData(infile)
    return ''.join(content)


# --------------------------------------------------------------------------
def main():
    viewer = r"output_template.html"
    error = r"testplan_error.html"

    # get two input parameters
    form = cgi.FieldStorage()

# TESTING lINES
# Uncomment these lines then you can run the script from the command line
#    for name, value in {
#                        "WEBROOT" : "/var/www/html/sheila/regression/",
#                        "infile" : "/home/sneedham/regressionV2/runner/input//SENTALT",
#                        "dateDir" : "2019/09/02",
#                        "SYS1" : "mdbap-preprod",
#                        "SYS2" : "mdbap-preprod",
#                        "VER1" : "5.20.0",
#                        "VER2" : "5.19.0",
#                        "OUTDIR" : "/var/www/html/sheila/regression/output/"}.items():
#        form.list.append(cgi.MiniFieldStorage(name, value))
#
#  END oF TESTING LINES

    webRoot = form.getvalue('WEBROOT')
    infile = form.getvalue('infile')
    dateDir = form.getvalue('dateDir')
    sys1 = form.getvalue('SYS1')
    sys2 = form.getvalue('SYS2')
    ver1 = form.getvalue('VER1')
    ver2 = form.getvalue('VER2')
    outdir = form.getvalue('OUTDIR')
    # only need last part of infile location
    infile = os.path.basename(infile)
    out1 = readData(outdir + '/' + 
                    dateDir + '/' + 
                    infile + '_0.out')
    out2 = readData(outdir + '/' + 
                    dateDir + '/' + 
                    infile + '_1.out')

    # compare files using diff
    d = difflib.HtmlDiff()
    hdr1 = sys1 + '/' + ver1   
    hdr2 = sys1 + '/' + ver2    
    out3 = d.make_file(out1, out2, fromdesc=hdr1, todesc=hdr2)

    # get log files
    log1 = readInp(outdir + '/' +
                   dateDir + '/' + infile + '_0.log')
    log2 = readInp(outdir + '/' +
                   dateDir + '/' + infile + '_1.log')
    templateVars = {"out3": out3,
                    "log1": log1,
                    "log2": log2,
                    "sys1": sys1,
                    "sys2": sys2}
    hdr = "Content-Type: text/html;charset=utf-8\n\n"
    print(jinja_render(webRoot, viewer, hdr, **templateVars))


# --------------------------------------------------------------------------
if __name__ == "__main__":
    
    main()
