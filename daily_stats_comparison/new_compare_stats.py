#!/usr/bin/env python
#-----------------------------------------------------------------------
#
# PROGRAM       : new_compare_stats.py
#
# PURPOSE       : From daily stats on prod and preprod this program takes the
#                 left-hand frames and analyses the stats, producing a page
#                 of differences etc. A summary is at the top for pasting
#                 into the Twiki 
#-----------------------------------------------------------------------
# Log
# July 2016: Stan Kellett added check for no prod data and there 
#            being pre-prod data
#
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2016 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom.
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#-----------------------------------------------------------------------
import re
import sys
import os
import urllib
import datetime as dt
 
prodURL=r"http://mdbdb-prod/moods/new_stats/"
prepURL=r"http://mdbdb-preprod/moods/new_stats/"
baseDir=r"/home/h01/usmdb/public_html/moods/misc/compare_stats_archive"
 
#-----------------------------------------------------------------------
# Function to read a daily stats summary page and return a dictionary
# keyed on datatype with the number of obs for that datatype.
#-----------------------------------------------------------------------
def readdata(filename):
  stats = {}
  try:
    inp=urllib.urlopen(filename)

  except IOError:
    print("Error opening file "+filename)
    sys.exit(1)
    
  for line in inp:
    if line.startswith("<TD>"):    # it's a datatype line - we want it

      # find the datatype name
      dtp = re.search('html#(.+?)" target', line)
      if dtp:
        dataType = dtp.group(1)
 
      # find the number of obs - they are either in BLUE or bold RED
      val = re.search('BLUE>(.+?)<', line) 
      if val:
        nobs = val.group(1)
      else:
        val = re.search('RED><B>(.+?)<', line)
        if val:
          nobs = val.group(1)

      # add it to the dictionary
      stats[dataType] = int(nobs)

  return stats
  
#-----------------------------------------------------------------------
# Main program begins here
#-----------------------------------------------------------------------
summary="output1.html" 
full="mdb_Daily_stats.html"

# Do some date manipulation to get today's and yesterday's date, used for the output file name
# and for the link to yesterday's page
now = dt.datetime.now()
datestr = now.strftime("%H:%M %d-%m-%Y")
outDir = baseDir + now.strftime("/%Y/%m/")
if not os.path.exists(outDir):
    os.makedirs(outDir)
outFile = outDir + now.strftime("%d") + ".html"

yesterday = now - dt.timedelta(days=1)
yesterdaysFile = "/moods/misc/compare_stats_archive/" + yesterday.strftime("%Y/%m/%d") + ".html"

preValues = readdata(prepURL+summary)
prodValues = readdata(prodURL+summary)

# Create a list of unique data types from both dictionaries
allTypes = sorted(set(preValues.keys() + prodValues.keys()))

moreOnPre = []
moreOnProd = []
missingOnPre = []
missingOnProd = []
tableRows = []

for dataType in allTypes:
  str1 = dataType
  str4 = 'normal'  # default Booststrap context
  if dataType in prodValues:
    str2 = str(prodValues[dataType])
    if dataType in preValues:
      if prodValues[dataType] == preValues[dataType]:
        str3 = 'Same'
      elif prodValues[dataType] < preValues[dataType]:
        str3 = str(preValues[dataType])
        str4 = 'warning'
        diff = preValues[dataType] - prodValues[dataType]
	
# add check to make sure that prodValues[dataType] includes data
	if prodValues[dataType] == 0:
	  pct = 0.0   
          moreOnPre.append(dataType + ' (' + str(diff) + ', ' + 'No data on prod)')
	else:
          pct = (float(diff)/prodValues[dataType]) 	   
          moreOnPre.append(dataType + ' (' + str(diff) + ', ' + "{0:.2f}".format((pct)*100) + '%)')
	  
      else:
        str3 = str(preValues[dataType])
        str4 = 'warning'
        diff = prodValues[dataType] - preValues[dataType]
        pct = (float(diff)/prodValues[dataType])
        moreOnProd.append(dataType + ' (' + str(diff) + ', ' + "{0:.2f}".format((pct)*100) + '%)')
    else:
      missingOnPre.append(dataType)
      str3 = 'Missing'
      str4 = 'info'
    if prodValues[dataType] == 0:
      str4 = 'danger'
  else:
    missingOnProd.append(dataType)
    str2 = 'Missing - new?'
    str3 = str(preValues[dataType])
    str4 = 'info'
  
  tableRow = (str1, str2, str3, str4)
  tableRows.append(tableRow)
  
# Write the HTML page
if preValues and prodValues:
  oput=open(outFile,"w")

  html_start = """<!DOCTYPE html>
  <html>
  <head>
    <title>MOODS daily stats comparison</title>
    <!--Bootstrap core CSS -->
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
    <link rel="stylesheet" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
  </head>
  <body>
    <div class="container">
    <h2>Daily Stats Comparison (run at """+datestr+""")</h2>
  <div class="row">
    <div class="col-xs-3">
      <div><a class="btn btn-danger btn-lg btn-block" href=""" + prodURL + full + """ role="button">Prod Daily Stats</a></div>
    </div>
    <div class="col-xs-3">
      <div><a class="btn btn-warning btn-lg btn-block" href=""" + prepURL + full + """ role="button">Pre-Prod Daily Stats</a></div>
    </div>
    <div class="col-xs-3">
      <div><a class="btn btn-success btn-lg btn-block" href=""" + yesterdaysFile + """ role="button">This page yesterday</a></div>
    </div>
    <div class="col-xs-3">
      <div><a class="btn btn-info btn-lg btn-block" href="/moods/misc/compare_stats_archive" role="button">Archive of this page</a></div>
    </div>
  </div> <!-- /.row -->
  <hr>
  """
  oput.write(html_start)

# Print the summary, for copy & pasting into the Twiki
  oput.write('<h3>Summary (copy and paste into Twiki)</h3>\n')
  if len(moreOnPre) > 0:
    oput.write('<p>More on Pre : ' + '; '.join(moreOnPre) + '</p>\n')
  if len(moreOnProd) > 0:
    oput.write('<p>More on Prod: ' + '; '.join(moreOnProd) + '</p>\n')
  if len(missingOnPre) > 0:
    oput.write('<p>Missing on Pre : ' + ', '.join(missingOnPre) + '</p>\n')
  if len(missingOnProd) > 0:
    oput.write('<p>Missing on Prod: ' + ','.join(missingOnProd) + '</p>\n')

# Now print the table rows for each data type...
  html_tab = """
  <h3>Detail</h3>
  <table class="table table-bordered table-condensed">
    <tr><th>Data Type</th><th>Count on Prod</th><th>Count on Pre-Prod</th></tr>
  """
  oput.write(html_tab)

  for row in tableRows:
    oput.write('<tr class="' + str(row[3]) + '">')
    oput.write('<td>' + str(row[0]) + '</td><td>' + str(row[1]) + '</td><td>' + str(row[2]) + '</td>')
    oput.write('</tr>\n')

  html_end = """
  </table>
  <p  class="text-center">Created by script """ + sys.argv[0] + """ running as user """ + os.getenv('USER') + """ on """ + os.uname()[1] + """</p>
  </div> <!-- /.container -->
  </body>
  </html>\n"""
  oput.write(html_end)

# Recreate the symlink to today's file
  os.unlink("/home/h01/usmdb/public_html/moods/misc/new_compare_stats.html")
  os.symlink(outFile,"/home/h01/usmdb/public_html/moods/misc/new_compare_stats.html")
  oput.close()
