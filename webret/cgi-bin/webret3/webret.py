''' This version uses input elements list and the code from metdb_retrieval
    to produce a table of decoded values for LNDSYN.
    Fixed list of elements in lndsyn_elements.txt.
    Bit of a bodge to get the process_function routine to work - scope issues
    when trying to use the version imported from get_data...?
'''
import metdb
import datetime
import cgi
import numpy as np
import sys
import importlib
import traceback
from jinja2 import FileSystemLoader, Environment
sys.path.append('/var/moods/metdb_utils/web')
from jinja_render import jinja_render

sys.path.append('/home/sneedham/metdb-misc/metdb_retrieval/python')
from get_data import Elements
import servicehub 
from unit_utils import *
MDI = np.ma.masked
# -------------------------------------------------------------------
def process_function(expression, obs, i, number=None):
    """Get arguments for the given function, evaluate it and
       returns the results as a string representation.

       obs is a numpy masked array as returned by the metdb call
       (therefore it can be indexed by element name).
    """
    global sites

# get the variable names from the expression...
    args_list = Elements.parse_elements(expression)
    args = []
    value = ''

# ...and put them in an ordered list

    for a in args_list:
        if '%' not in a:
            if number >= 0:
                args.append(obs[a][i][number])
            else:
                args.append(obs[a][i])
        else:
            args.append(a[1:])
# get the function name, if there is one

    parts = expression.split('(')
    if len(parts) == 1:
        func = None
    else:
        func = parts[0]
    if func:
        try:
            f = eval(func)
            value = f(*args)

        except:
            print 'Error calling function', func, args
            traceback.print_exc()
            sys.exit(2)
    else:
        num = args[0]
        if num is not MDI:
            value = str(num)
    return value



# Base locations

webRoot=r"/var/www/html/webret3/"
viewer=r"web_template.html"

np.set_printoptions(threshold=np.inf)

contact = 'sheila.needham@metoffice.gov.uk'

# get form fields 

form = cgi.FieldStorage()
subtype = form.getvalue("subtype")
d1 = form.getvalue("startdate")
t1 = form.getvalue("starttime")
d2 = form.getvalue("enddate")
t2 = form.getvalue("endtime")
fields = form.getlist("element")


# construct the metdb request

start_time = d1[0:4] + d1[5:7] + d1[8:10] + "/" + t1[0:2] + t1[3:5] + "Z"
end_time = d2[0:4] + d2[5:7] + d2[8:10] + "/" + t2[0:2] + t2[3:5] + "Z"

platform = form.getvalue("platform")

keywords=['START TIME ' + start_time,
          'END TIME ' + end_time,
          'PLATFORM ' + platform]

# create elements list

if subtype == "LNDSYN":
    elements_file = "/var/www/cgi-bin/webret3/lndsyn_elements.txt"
elements = Elements(elements_file)
elements_list = elements.get_element_names()

# Return the obs from MetDB/MOODS.
obs = metdb.obs(contact, subtype, keywords, elements_list)

# Output

csv_list = []
for i in range(len(obs)):
    output_csv = {}
    for k, v in elements.element_map.iteritems():
        if k in fields:
            expression = v[1]
                    # check if this is a layer element by looking for a
                    # prefix on the key
            if k[0] == '_':
                numStr = k.split('_')[1]
                num = int(numStr) - 1
                output_csv[k] = process_function(expression,
                                             obs, i,
                                             number=num)
            else:
                output_csv[k] = process_function(expression, obs, i)
    # end of loop over elements
    csv_list.append(output_csv)

# end of loop over obs
title = fields

# title =  elements.fields

templateVars = {"obs" : csv_list,
                "title" : title,
                "subtype" : subtype}

hdr = "Content-Type: text/html;charset=utf-8\n\n"
print jinja_render(webRoot, viewer, hdr, **templateVars)

