#!/opt/scitools/environments/default/2019_02_27/bin/python

'''
`search_station.py` looks up station details based on name or ID.

Does partial matches in the ID field or name field of WMO and/or
ICAO station lists from moods.

Form input:
   * tsearch - type of search which is either icao, wmo or both
   * pattern - to be searched for.
'''

import cgi
import cgitb
from jinja2 import FileSystemLoader, Environment
import urllib.request
import urllib.parse
import urllib.error
from urllib.error import HTTPError, URLError
import re
import sys

cgitb.enable()
METAR_REQ = ('http://mdbdb-prod/cgi-bin/moods/webret.pl?'
             'subtype=METARS&submit=Retrieve+Latest+Report&stn01=')
LNDSYN_REQ = ('http://mdbdb-prod/cgi-bin/moods/webret.pl?'
              'subtype=LNDSYN&submit=Retrieve+Latest+Report&stn01=')


def render(directory, template_name, **kwargs):
    loader = FileSystemLoader(directory)
    env = Environment(loader=loader)
    template = env.get_template(template_name)
    output = "Content-Type: text/html;charset=utf-8\n\n" +\
             template.render(**kwargs)
    return output


def cgiFieldStorageToDict(form):
    """ Get a plain dictionary rather than the '.value' system used by the
    cgi module's native fieldStorage class. """
    params = {}
    for key in form.keys():
        params[key] = form.getvalue(key, "")
    return params


class Stations():
    """Represents station details.

    List items: As in the station files
      * icao_id: may be missing
      * wmo_id: may be missing
      * lat: decimal
      * lon: decimal
      * ht1: sensor height
      * ht2: station height
      * name: site name
      * link: link to webret for this station

    """

    def __init__(self):
        """Initialise dictionaries for each search possibility"""

        self.icao_dict = {}
        self.wmo_dict = {}
        self.icao_name_dict = {}
        self.wmo_name_dict = {}

    def loadData(self, URL):
        """Access files via URL"""

        response = urllib.request.urlopen(URL)
        text = response.read().decode()
        extract = re.search(r'<pre>(.*?)</pre>', text, re.I | re.DOTALL)
        ngrps = extract.lastindex
        if extract and ngrps == 1:
            lines = extract.group(1).split('\n')
        else:
            raise Exception(f'Unable to parse station file {URL}')

        return lines

    def getWmo(self):
        """Read MetDB abreviated station list"""

        URL = ('http://mdbdb-prod/cgi-bin/moods/printfile.pl?'
               'fullname=/var/moods/data/STATIONS/abrv_stnlist')
        try:
            lines = self.loadData(URL)
        except Exception as e:
            raise Exception(f'Cannot load WMO file {URL} ')

        if len(lines) < 10:
            raise Exception(f'Error reading WMO file')
        lines = lines[2:]

        stations = []
        for line in lines:
            if line.strip() == '':
                break
            icao = ''
            wmo = line[1:6]
            lat = line[9:16]
            lon = line[17:25]
            ht1 = line[30:34]    # Sensor height
            ht2 = line[37:42]    # Station height
            name = line[57:].strip()
            link = LNDSYN_REQ + wmo

            stations.append((icao, wmo, lat, lon, ht2, ht1, name, link))

        # set up dict with keys on both WMO ID and NAME
        for station in stations:
            self.wmo_dict[station[1]] = station
            self.wmo_name_dict[station[6]] = station

    def getIcao(self):
        """Read ICAO station list"""

        URL = ('http://mdbdb-prod/cgi-bin/moods/printfile.pl?'
               'fullname=/usr/local/moods/tables/icao_list')
        try:
            lines = self.loadData(URL)
        except Exception as e:
            raise Exception(f'Cannot load ICAO file {URL}')

        stations = []
        if len(lines) < 10:
            raise Exception(f'Error reading ICAO file')
        for line in lines:
            if line.strip() == '':
                break
            icao = line[0:4]
            wmo = line[5:10]
            lat = line[11:18]
            lon = line[19:27]
            ht1 = line[28:32]   # Station height
            ht2 = line[33:37]   # Sensor height
            name = line[38:].strip()
            link = METAR_REQ + icao

            stations.append((icao, wmo, lat, lon, ht1, ht2, name, link))
        # set up dict with keys on both ICAO ID and NAME
        for station in stations:
            self.icao_dict[station[0]] = station
            self.icao_name_dict[station[6]] = station

    def lookup_icao(self, pattern):

        result = [value for key, value in self.icao_dict.items()
                  if key.startswith(pattern.upper())]
        return result

    def lookup_wmo(self, pattern):

        result = [value for key, value in self.wmo_dict.items()
                  if key.startswith(pattern.upper())]
        return result

    def lookup_wmo_name(self, pattern):

        result = [value for key, value in self.wmo_name_dict.items()
                  if pattern.upper() in key]
        return result

    def lookup_icao_name(self, pattern):

        result = [value for key, value in self.icao_name_dict.items()
                  if pattern.upper() in key]
        return result


if __name__ == "__main__":

    webRoot = r"/var/www/html/stations/"
    viewer = "stnlist_template.html"
    error = "stnerr_template.html"

# get user input fields
    fields = cgiFieldStorageToDict(cgi.FieldStorage())
    tsearch = fields.get('tsearch', '')
    pattern = fields.get('pattern', '')

# this bit for testing from the command line
#    fields = {}
#    fields['tsearch'] = 'icao'
#    fields['pattern'] = 'EG'
#    tsearch = fields['tsearch']
#    pattern = fields['pattern']

# initialise the stnlist container
    stnlist = Stations()
    try:
        # and populate it from either WMO, ICAO or both
        if tsearch == 'icao' or tsearch == 'both':
            stnlist.getIcao()

        if tsearch == 'wmo' or tsearch == 'both':
            stnlist.getWmo()

        if pattern == '' or not pattern.isalnum():
            raise Exception(f'Invalid search pattern {pattern}')

# Lookup the station details
        if tsearch == 'icao':
            result = stnlist.lookup_icao(pattern)
        elif tsearch == 'wmo':
            result = stnlist.lookup_wmo(pattern)
        elif tsearch == 'both':
            result = stnlist.lookup_wmo_name(pattern)
            result.extend(stnlist.lookup_icao_name(pattern))
        else:
            raise Exception(f'Invalid input {tsearch} {pattern}')

        if len(result) == 0:
            fields["errcode"] = "No stations match your request"
        else:
            fields["errcode"] = ""

        fields["stations"] = result
        templateVars = {"fields": fields}
        print(render(webRoot, viewer, **templateVars))

    except Exception as e:
        templateVars = {"errcode": str(e)}
        print(render(webRoot, error, **templateVars))
        sys.exit(0)
