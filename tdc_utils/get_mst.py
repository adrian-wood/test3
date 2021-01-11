#!/opt/scitools/environments/production/2019_06_12/bin/python
"""
`get_mst.py` retrieves the current Measurement Site Table from HE.

This only runs on a Linux desktop and needs a .netrc file for
authentication.  Contact Sheila for details.

The python 3 environment is hard-coded due to known issues with the
requests module in the default Python 3 environment (at time of
writing).

Usage:
  1) Make sure your ~/.netrc file contains the required details
  2) Get a copy of the script from the metdb-misc repo in Bitbucket
     tdc_utils/get_mst.py
  3) ./get_mst.py

This will download the current MST file and write it out to the
current working directory as MST_<yyyymmdd>.xml.  It can take a few
minutes to run.

Author:
   Sheila Needham

Date:
   21 Nov 2019

"""

import requests
from requests.auth import HTTPBasicAuth
import xml.dom.minidom
import netrc
import sys
import time

resource = 'swis.highwaysengland.co.uk'
URL = ('https://www.swis.highwaysengland.co.uk/webservices'
       '/mstdistribution/mstdistributionservice.asmx')

# Get username and password from /netrc file
try:
    info = netrc.netrc().authenticators(resource)
    if info is not None:
        username = info[0]
        password = info[2]
    else:
        print(f'No authentication for {resource}')
        print('Only run on mdb-apps-test as moodsf')
        sys.exit(1)

except (IOError, netrc.NetrcParseError) as err:
    print(f'Error parsing .netrc {str(err)}')
    sys.exit(1)

payload = """<?xml version="1.0" encoding="utf-8"?>
             <soap12:Envelope
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xmlns:xsd="http://www.w3.org/2001/XMLSchema"
               xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">
             <soap12:Body />
            </soap12:Envelope>"""
headers = {"Content-Type": "text/xml; charset=UTF-8",
           "Content-Length": str(len(payload))}

# Make the request to download the file
try:
    print('Downloading...')
    response = requests.post(URL,
                             data=payload,
                             timeout=5,
                             headers=headers,
                             auth=HTTPBasicAuth(username, password))
    response.raise_for_status()

except requests.exceptions.HTTPError as http_err:
    print(f'HTTP error occurred: {http_err}')
    sys.exit(1)
except Exception as err:
    print(f'Other error occurred: {err}')
    sys.exit(1)

# Check it's downloaded some valid XML and format it
try:
    dom = xml.dom.minidom.parseString(response.text)
    pretty_xml = dom.toprettyxml()
except xml.parsers.expat.ExpatError as err:
    print(f'Error parsing request: {str(erro)}')
    sys.exit(1)

# Now write it out
outfile = 'MST_' + time.strftime("%Y%m%d") + '.xml'
try:
    f = open(outfile, 'w')
    f.write(pretty_xml)
    f.close()
except IOError as err:
    print(f'Error writing file {outfile}')
    sys.exit(1)

print(f'Latest MST successfully downloaded to {outfile}')
