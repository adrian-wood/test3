#!/usr/bin/env python
'''Produce a web page comparing regression test output and log
   files.  Script is invoked by a link from the main regression
   test summary page. e.g.
     OutputDisplay.py?infile=$INDIR/OPENROAD&dateDir=2018/05/15

   The environment is initialised from a config file - SetEnv.txt.
'''
import cgi
import cgitb
import sys
import os
import os.path
import difflib
import metdbEnv
sys.path.append('/var/moods/metdb_utils/web')
from jinja_render import jinja_render
cgitb.enable()


# ---------------------------------------------------------------------------
def readData(filename):
    inp = open(filename, 'r')
    lines = inp.readlines()
    inp.close()
    return lines


# --------------------------------------------------------------------------
def readInp(infile):
    content = readData(infile)
    return ''.join(content)


# --------------------------------------------------------------------------
def main():
    viewer = r"output_template.html"
    error = r"testplan_error.html"
    metdbEnv.setEnv('SetEnv.txt')
    webRoot = os.environ.get('WEBROOT', '.')

    # get two input parameters
    form = cgi.FieldStorage()
    infile = form.getvalue('infile')
    dateDir = form.getvalue('dateDir')

    # only need last part of infile location
    fullpath = metdbEnv.subEnv(infile)
    infile = os.path.basename(fullpath)
    sys1 = os.environ.get('SYS1')
    sys2 = os.environ.get('SYS2')
    outdir = os.environ.get('OUTDIR')

    out1 = readData(outdir + '/' + sys1 + '/' +
                    dateDir + '/' + infile + '.out')
    out2 = readData(outdir + '/' + sys2 + '/' +
                    dateDir + '/' + infile + '.out')

    # compare files using diff
    d = difflib.HtmlDiff()
    out3 = d.make_table(out1, out2, fromdesc=sys1, todesc=sys2)

    # get log files
    log1 = readInp(outdir + '/' + sys1 + '/' +
                   dateDir + '/' + infile + '.log')
    log2 = readInp(outdir + '/' + sys2 + '/' +
                   dateDir + '/' + infile + '.log')
    templateVars = {"out3": out3,
                    "log1": log1,
                    "log2": log2,
                    "sys1": sys1,
                    "sys2": sys2}
    hdr = "Content-Type: text/html;charset=utf-8\n\n"
    print jinja_render(webRoot, viewer, hdr, **templateVars)


# --------------------------------------------------------------------------
if __name__ == "__main__":
    main()
