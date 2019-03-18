''' Very basic cgi-script for web retrieval of raw report text.
    Only caters for LNDSYN, METARS and TAFS.
    Platform is required.
    No validation of input.
'''
import metdb
import datetime
import cgi
import numpy as np
from jinja2 import FileSystemLoader, Environment

def render(directory, template_name, **kwargs):
    loader = FileSystemLoader(directory)
    env = Environment(loader=loader)
    template = env.get_template(template_name)
    output = "Content-Type: text/html;charset=utf-8\n\n" + template.render(**kwargs)
    return output

# Base locations

webRoot=r"/var/www/html/webret/"
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

# construct the metdb request

start_time = d1[0:4] + d1[5:7] + d1[8:10] + "/" + t1[0:2] + t1[3:5] + "Z"
end_time = d2[0:4] + d2[5:7] + d2[8:10] + "/" + t2[0:2] + t2[3:5] + "Z"

platform = form.getvalue("platform")

keywords=['START TIME ' + start_time,
          'END TIME ' + end_time,
          'PLATFORM ' + platform]

# create elements list

if subtype == "LNDSYN":
    report_text = 'RPRT_TEXT'
elif subtype == "METARS":
    report_text = 'MTR_RPT_TXT'
else:
    report_text = 'TAF_RPT_TXT'
elements = [report_text]


# Return the obs from MetDB/MOODS.
obs = metdb.obs(contact, subtype, keywords, elements)

# Output

templateVars = {"obs" : obs[report_text],
              "subtype" : subtype}
print render(webRoot,viewer,**templateVars)

