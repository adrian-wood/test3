import sys
import os
import calendar
import time
import datetime as dt
miscPath = os.environ.get('MISC_BASE_DIR', '/home/ajmoorho/metdb-misc')
templatePath = miscPath + '/mdb_activity/templates/'
datatypeTemplate = 'user_contact_template.html'
sys.path.append(os.path.join(miscPath, 'metdb_utils'))
import mdb_files.RetrievalTable as RT
import mdb_files.DataAccessLog as DA
from web.jinja_render import jinja_render
from web.daily_archive import daily_archive
full_path = '/home/ajmoorho/metdb-misc/metdb_utils/mdb_files/test_data/data_access.log'
full_path = '/var/www/html/mdb_activity/data_access_logs/mdbapus-prod/2020/01/data_access.log-20200131'
print('  Reading file', full_path)
with open(full_path, errors='ignore') as f:
    da_file = DA.DataAccessLog(f)

# print(da_file.count_by_userid)

# print(da_file.count_by_datatype.keys())

byUseridRows = []
byContactRows = []

for user in da_file.userid_contact.keys():
    useridRow = {"userid": user,
                 "count": da_file.count_by_userid[user],
                 "contacts": da_file.userid_contact[user]}
    byUseridRows.append(useridRow)

for contact in da_file.contact_userid.keys():
    contactRow = {"contact": contact,
                 "count": da_file.count_by_contact[contact],
                 "userids": da_file.contact_userid[contact]}
    byContactRows.append(contactRow)

# Create the keyword:arguments dictionary for the Jinja template
templateVars = {"byUseridRows": sorted(byUseridRows, key=lambda i: i['count'], reverse=True),
                "byUserTotal": da_file.retrieval_count,
                "byContactRows": sorted(byContactRows, key=lambda i: i['count'], reverse=True),
                "byContactTotal": da_file.retrieval_count,
                "server": 'mdbapus-prod',
                "logDate": '2020/01/31',
                "creator": sys.argv[0],
                "runDate": time.strftime("%c"),
                "runner": os.getenv('USER'),
                "where": os.uname()[1]}

# Render the HTML from template, create a page
htmlout = jinja_render(templatePath, datatypeTemplate,
                        **templateVars)
outFile = '/var/www/html/andy/user_contact.html'
with open(outFile, "w") as outp:
    outp.write(htmlout)