#!/usr/local/sci/bin/python2.7
"""Produce a summary page of daily stats for GRIB, NetCDF, HDF5 and XML jobs.

  (c) Crown copyright 2016, the Met Office.
  Reads a configuration file containing details of which MetDB jobs to check
  and what the criteria for success is, where appropriate.
  Scrapes the output from these jobs and produces a summary page, using a 
  Jinja template.
  Can be run anytime but usually will be run by cron once per day in the morning.
"""

import sys
import os
import urllib2
import logging
import getopt
from ConfigParser import SafeConfigParser
from datetime import datetime,timedelta

sys.path.append('/home/h01/usmdb/metdb_utils/web')
from jinja_render import jinja_render
from daily_archive import daily_archive

def parse_storedf(jobname,stdout_files,server_url):
  """Check the output of a list of storedf job(s).

  Obtain the output from each of the stdout files supplied in stdout_files for
  the supplied jobname from the supplied server.
  The criteria for success or otherwise is that for every data type that
  the job is processing, there should be some read and they should all
  be stored successfully.
  If some have been rejected, then flag that, and if any datatypes
  received any data.

  Args:
    jobname: name of the job to look for e.g. MDBBDF1
    stdout_files: a list of stdout files to examine
    server_url: which server to get the info from, e.g. mdbdb-prod, mdbdb-preprod

  Returns:
    a_result: a dictionary of text, popover values, status etc to be used by the Jinja template.
  """
  logging.debug('Parsing storedf for job ' + jobname + ', files = ' + str(stdout_files))
  popover_title = "Start time, counts:"
  job_status = "success" # default
  job_text = "OK" # default
  popover_title = "Start time, counts:"
  popover_text = []

  for a_file in stdout_files:
    logging.debug('a_file = ' + a_file)
    ts = a_file.split('_')[2][8:14]
    timestamp = ts[0:2] + ":" + ts[2:4] + ":" + ts[4:6]
    stdout=r"http://" + server_url + "/cgi-bin/moods/printfile.pl?fullname=/var/moods/logs/" + jobname + "/" + a_file
    response = urllib2.urlopen(stdout)
    s = response.read()

    # process startup text - we're definiteley going to get this...
    start = '----   ---------   -------------------'
    end = 'DETAILS OF SCHEMAS'
    startup = s[s.find(start)+len(start):s.rfind(end)].rstrip()
    # get a list of all the daatypes this jobs is handling
    datatypes = startup.split()[1::3] # every 3rd word starting with the 2nd is a datatype :-)
    if popover_text:
      popover_text.append('\n' + timestamp)
    else:
      popover_text.append(timestamp)
    popover_text.append('\t'.join(['datatype', 'read', 'stored', 'rejected']))

    # process shutdown text
    shutdown_text = 'SUMMARY OF MESSAGES PROCESSED'
    if shutdown_text in s:

      # we've shutdown successfully; extract and process the counts
      start = '---------    ------  ------  ------'
      end   = 'TOTALS:'
      output = s[s.find(start)+len(start):s.rfind(end)].rstrip()
      datatype_counts = {}

      for x in output.split('\n'):
        if x and '----' not in x: # we've got a line to parse
	  datatype_counts[x.split()[0]] = (x.split()[1], x.split()[2], x.split()[3])

      for datatype in datatypes:
        if datatype in datatype_counts:
	  counts = datatype_counts[datatype]
	  logging.debug('counts = ' + ' '.join(counts))
          popover_text.append('\t'.join([datatype.ljust(8), '\t'.join(counts)]))
	else: # not found in results
          popover_text.append('\t'.join([datatype.ljust(8), "Missing"]))
	  job_text = "Types Missing"
	  job_status = "warning"

    else:  # shutdown text not found, so must have aborted.
      popover_text.append(timestamp + ' - no counts')
      job_status = "danger"

  a_result = dict(job_status=job_status, popover_title=popover_title, popover_text='\n'.join(popover_text), job_text=job_text)
  return a_result

def parse_gribdat(jobname, stdout_files, expected, server_url):
  """Check the output of a list of gribdat job(s).

  Obtain the output from each of the stdout files supplied in stdout_files for
  the supplied jobname from the supplied server.
  The criteria for success or otherwise is that the total of files
  processed in the day (which might be > 1 job) should match the number
  supplied in the 'expected' parameter, and that number should have
  been 'copied'.
  If there is a different number, or not all have been copied, then
  flag as appropriate.

  Args:
    jobname: name of the job to look for e.g. MDBBDF1
    stdout_files: a list of stdout files to examine
    expected: the expected count of files processed in one day
    server_url: which server to get the info from, e.g. mdbdb-prod, mdbdb-preprod

  Returns:
    a_result: a dictionary of text, popover values, status etc to be used by the Jinja template.
  """

  logging.debug('Parsing gribdat for job ' + jobname + ', files = ' + str(stdout_files))
  popover_title = "Start time - read / copied"
  total_gribs_read = 0
  total_gribs_copied = 0
  popover_text = []
  job_status = "success"  # default

  for a_file in stdout_files:
    logging.debug('a_file = ' + a_file)
    ts = a_file.split('_')[2][8:14]
    timestamp = ts[0:2] + ":" + ts[2:4] + ":" + ts[4:6]
    stdout=r"http://" + server_url + "/cgi-bin/moods/printfile.pl?fullname=/var/moods/logs/" + jobname + "/" + a_file
    response = urllib2.urlopen(stdout)
    s = response.read()

    # We are looking for the last line of output for the "nn GRIB DATASETS READ and nn COPIED",
    # which is actually the 4th last line of the HTML response.
    lines = s.split('\n')  # split the output on the new line char
    output = lines[len(lines) - 4]

    if "GRIB DATA SETS READ" in output:  # this should be what we want
      logging.debug('Found something in ' + a_file)
      gribs_read = output.split()[0]
      gribs_copied = output.split()[6]
      popover_text.append(timestamp + ' - ' + gribs_read + ' / ' + gribs_copied)
      total_gribs_read += int(gribs_read)
      total_gribs_copied += int(gribs_copied)
    else:       # this file does not have normal shutdown messages, so must have aborted.
      popover_text.append(timestamp + ' - no counts')
      job_status = "danger"

  # are the read / copied totals OK?
  job_text = str(total_gribs_read)
  if total_gribs_copied != int(expected):
    job_status = "warning"
    if total_gribs_read != int(expected):
      job_status = "danger"
  logging.debug('popover_text = ' + str(popover_text))
  a_result = dict(job_status=job_status, popover_title=popover_title, popover_text='\n'.join(popover_text), job_text=job_text)
  return a_result

def get_stdout_matches(jobname,dates,server_url):
  """Provide matching stdout filenames for a list of dates.

  For the supplied jobname, use listlogdir.pl to obtain all the available 
  output, and use the lxml toolkit to parse the html returned.  Match the 
  resulting stdout files with each of the supplied date(s).
  Return a dictionary keyed on date with values of the matching stdout filenames.

  Args:
    jobname: name of the job to look for e.g. MDBBDF1
    dates: list of dates we're interested in
    server_url: which server to get the info from, e.g. mdbdb-prod, mdbdb-preprod

  Returns:
    stdout_matches: a dictionary keyed on date with values of matching stdout files.
  """

  import lxml
  import requests
  from lxml import html

  stdout_matches = {}
  stdout_files = []
  basedir = '/var/moods/logs/'
  payload = {'baselogdir': basedir, 'jobname': jobname}

  # get all the stdout files that are available for this job
  listurl = 'http://' + server_url + '/cgi-bin/moods/listlogdir.pl'
  r = requests.get(listurl, params=payload)
  tree = lxml.html.fromstring(r.content)

  for elt in tree.iter('a'):
    text=elt.text_content()
    if text.endswith('.stdout'):
      stdout_files.append(text)
  
  # now match the files with dates...
  for date in dates:
    date = date.translate(None,'/') # stdout file name has date in a different format
    matches = [s for s in stdout_files if date in s]
    logging.debug(" ".join(['matches for',date,'are',str(matches)]))
    stdout_matches[date] = matches

  return stdout_matches

def main():
  """Main program"""

  # Obtain the name of the configuration file, must be supplied with -c <file>
  config_file = ''
  try:
    opts, args = getopt.getopt(sys.argv[1:],"hc:")
  except getopt.GetoptError:
    print sys.argv[0], " -c <configfile>"
    sys.exit(2)

  for opt, arg in opts:
    if opt == "-h":
      print sys.argv[0], " -c <configfile>"
      sys.exit()
    elif opt == "-c":
      config_file = arg

  if not config_file:
   print sys.argv[0], "must supply config file with -c"
   sys.exit(2)

  # Read the configuration
  logLevel = ''
  Config = SafeConfigParser()
  Config.read(config_file)
  webBase      = Config.get('General', 'webBase')
  archiveBase  = Config.get('General', 'archiveBase')
  server       = Config.get('General', 'server')

  if Config.has_option('General','logLevel'):  # logging level
    logLevel = Config.get('General','logLevel')
    numeric_level = getattr(logging, logLevel.upper(), None)
    if not isinstance(numeric_level, int):
      raise ValueError('Invalid log level: %s' % loglevel)
    logging.basicConfig(level=numeric_level)
  else:
    logging.basicConfig(level=logging.WARNING)  # default

  # Locals
  server_url     = "mdbdb-" + server
  jobViewerURL  = "http://" + server_url + "/moods/viewers/job_viewer.html"
  symLink       = webBase + server + "/non_BUFR_stats.html"
  archiveDir    = archiveBase + server
  archive_url = "/moods/misc/non_BUFR_stats_archive/" + server
  number_of_days = 5  # how far to go back
  template_name = "non_BUFR_stats_template.html"
 
  # Some date manipulation to get today and yesterday
  now = datetime.now()
  datestr = now.strftime("%H:%M %d-%m-%Y")
  yesterday = now - timedelta(days=1)
  yesterdaysPage = "/moods/misc/non_BUFR_stats_archive/" + server + "/" + yesterday.strftime("%Y/%m/%d") + ".html"

  # Create a list of date strings for web page, starting from yesterday
  dates = []
  for i in range(number_of_days):
    dates.append((now - timedelta(days=i+1)).strftime("%Y/%m/%d"))

  sections = [] # list for Jinja2

  # Process each 'Section' in the configuration file
  conf_sections = [x.strip() for x in Config.get('Layout','Sections').split(',')]

  for conf_section in conf_sections:
    jobs = []
    jobtype = Config.get(conf_section,'type')
    conf_items = Config.items(conf_section)

    for key,value in sorted(conf_items):
      if key.startswith('job'):

        if jobtype == 'gribdat':
          items = [x.strip() for x in value.split(',')]
          job_name = items[0]
	  expected_count = items[1]
        elif jobtype == 'gribdat_oneshot':
          items = [x.strip() for x in value.split(',')]
          job_name = items[0]
	  expected_count = items[1]
        elif jobtype == 'storedf':
          job_name = value

        job = dict(job_name=job_name)
        results =[]
        stdouts = get_stdout_matches(job_name, dates, server_url)
        logging.debug(" ".join(['stdouts for',job_name,'are',str(stdouts)]))
      
        for date,match_list in sorted(stdouts.iteritems()):
	  if match_list: 
            if jobtype == 'gribdat':
              a_result = parse_gribdat(job_name,match_list,expected_count,server_url)
            elif jobtype == 'storedf':
              a_result = parse_storedf(job_name,match_list,server_url)
	  else: # need an empty "result"
	    a_result = dict(job_status="", popover_title="", popover_text="", job_text="")

	  results.insert(0, a_result)

        job['results'] = results
        jobs.append(job)

    a_section = dict(title=Config.get(conf_section,'title'),freq=Config.get(conf_section,'freq'),dates=dates,jobs=jobs)
    sections.append(a_section)

  # get the current MONITOR.errorlog output
  monerr=r"http://" + server_url + "/cgi-bin/moods/printfile.pl?fullname=/var/moods/logs/MONITOR.errorlog"
  response = urllib2.urlopen(monerr)
  s = response.read()
  start = '<pre>'  # we want everything between the <pre> and </pre> tags
  end = '</pre>'
  monitor_errorlog = s[s.find(start)+len(start):s.rfind(end)].rstrip()

  # Create the keyword:arguments dictionary for the Jinja template
  templateVars={"runtime" : datestr,
    "yesterdays_page" : yesterdaysPage,
    "monitor_errorlog" : monitor_errorlog,
    "sections" :  sections,
    "creator" : sys.argv[0],
    "runner" : os.getenv('USER'),
    "where" : os.uname()[1],
    "server" : server,
    "archive_url" : archive_url,
    "server_url" : server_url
    }

  # Render the HTML from template, create a page and archive it
  htmlout=jinja_render(webBase,template_name,**templateVars)
  daily_archive(symLink,archiveDir,htmlout)

if __name__ == "__main__":
    main()
