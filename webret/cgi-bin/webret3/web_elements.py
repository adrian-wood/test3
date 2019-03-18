''' This populates an element selection table for LNDSYN.
'''
import cgi
import sys

from jinja2 import FileSystemLoader, Environment
sys.path.append('/var/moods/metdb_utils/web')
from jinja_render import jinja_render
sys.path.append('/home/sneedham/metdb-misc/metdb_retrieval/python')
from get_data import Elements


# Base locations

webRoot=r"/var/www/html/webret3/"
viewer=r"web_elements.html"


# get form fields 

form = cgi.FieldStorage()
subtype = form.getvalue("subtype")
form_fields = {}
for key in form.keys():
    form_fields[key] = str(form.getvalue(key))


elements_file = "/var/www/cgi-bin/webret3/lndsyn_elements.txt"
elements = Elements(elements_file)
rows = []
for k, v in sorted(elements.element_map.iteritems(),
                               key=lambda (k, v): v[0]):
    rows.append(k)


templateVars = {"rows" : rows,
                "subtype" : subtype,
                "form_fields" : form_fields }

hdr = "Content-Type: text/html;charset=utf-8\n\n"
print jinja_render(webRoot, viewer, hdr, **templateVars)

