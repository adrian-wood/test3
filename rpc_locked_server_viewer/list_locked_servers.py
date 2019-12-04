#!/opt/scitools/environments/default/2019_02_27/bin/python
import re
import cgi
from datetime import datetime
import cgitb; cgitb.enable()
import sys
import urllib
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

  inp=urllib.urlopen(filename)
  lines=inp.read() 
     
# regular expression to extract the whole table from the html text    
  rexpTable=r'<body>(.*)</body>'
  match=re.search(rexpTable,lines,re.DOTALL)
  if match:
    return match.group(1)
 
#--------------------------------------------------------------------------
def getUser():
  form=cgi.FieldStorage()
  user=form.getvalue("userid")
  if user == None:
    user=""
  return user

#--------------------------------------------------------------------------
def parseData(table,user):
  rawRows=table.split("<tr>")
  regextd=re.compile("</td>")
  regextr=re.compile("</td></tr>")
  rows=[]
  for r in rawRows:
    columns=r.split("<td>")
    if len(columns) > 1:
      prognum=columns[1][0:6]
      status=regextd.sub("",columns[2])
      time=regextd.sub("",columns[3])
      date=regextd.sub("",columns[4])
      lastCol=columns[5].split()
      userid=regextr.sub("",lastCol[0])
      if len(lastCol) > 1:
        timestamp=regextr.sub("",lastCol[1])
      else:
        timestamp=""
      if "locked" in status and user in userid:
        rows.append((userid,status,prognum,time,date,timestamp))
  return rows

#--------------------------------------------------------------------------
def main():
  operURL=r"http://mdbapus-prod/cgi-bin/moods/rpcprog.pl"
  webRoot=r"/var/www/html/viewers/"
  viewer=r"viewer_template.html"
  error=r"viewer_error.html"
  now=datetime.utcnow().strftime('%H:%M:%S')+'Z'

  # web=Webpage(webRoot)
  user=getUser()
  try:
    table=readData(operURL)
  except IOError as err:
    templateVars={"errorMsg" : err }
    print render(webRoot,error,**templateVars)
    sys.exit(1)
    
  rows=parseData(table,user)
  templateVars={"row" : rows, "count" : len(rows), "now" : now}
  print render(webRoot,viewer,**templateVars)

#--------------------------------------------------------------------------
if __name__ == "__main__":
   main()

