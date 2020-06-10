import requests
import json
import re
import sys
import io
import csv

from lxml import etree

BASE_URL = 'https://oscar.wmo.int/surface/rest/api'
SEARCH_API = BASE_URL + '/search/station?'
XML_DOWNLOAD_URL = BASE_URL + "/wmd/download/"

def extractPressureInfo(xml):
    # This is the set of namepsaces used within the XML
    ns = { 
    'wmdr':'http://def.wmo.int/wmdr/2017',
    'gml' : 'http://www.opengis.net/gml/3.2',        
    'xlink' : 'http://www.w3.org/1999/xlink',
    'xsi' : 'http://www.w3.org/2001/XMLSchema-instance',
    'om' : 'http://www.opengis.net/om/2.0'
    }
    
    # Xpath to get to the location of pressure sensor height field
    location = xml.findall('wmdr:facility'
                           '/wmdr:ObservingFacility'
                           '/wmdr:observation'
                           '/wmdr:ObservingCapability'
                           '/wmdr:observation'
                           '/om:OM_Observation'
                           '/om:procedure'
                           '/wmdr:Process'
                           '/wmdr:deployment'
                           '/wmdr:Deployment'
                           '/wmdr:deployedEquipment'
                           '/wmdr:Equipment' 
                           '/gml:identifier[@codeSpace="http://codes.wmo.int/wmdr/ObservedVariableAtmosphere/216"]'
                           '../wmdr:geospatialLocation'
                           '/wmdr:GeospatialLocation'
                           '/wmdr:geoLocation'
                           '/gml:Point'
                           '/gml:pos', ns)
    hp = None                                        
    if len(location) != 1:
        print('Warning - could not find pressure node')             
    else:             
        hp = location[0].text.split(' ')[2]
    
    return '{:.0f}'.format(int(float(hp))) if hp else ''


def convert_to_type(station):
    '''Decide whether station is surface, upper-air or both based
       on stationProgramsDeclaredStatuses.
       
       ** Not entirely sure the selection is right here** 
    '''

    
    remarks = station.get('stationProgramsDeclaredStatuses')
    sub_index = station.get('sub_index')
    stn_type = ''
    if sub_index == "1":
        stn_type = 'U/AIR'
    else:
        if remarks.find('GOS') > -1:
            stn_type = 'SURF'
        if (remarks.find('GUAN') > -1) or (remarks.find('RBSN(P)') > -1):
            if len(stn_type) > 0:
                stn_type += ' & U/AIR'
            else:
                stn_type = 'U/AIR'

    return stn_type


def search_oscar():
    '''Get station list from OSCAR.
    
    Args:
       * None
    
    Returns:
       * stations (dict) - keyed on WIGOS ID as string containing station details
                           returned from API plus some derived.
    Exceptions:
       * sys.exit(1) - API request fails
    '''

    # Set up variables for the station search API (this determines which stations make it into Vol A)

    variables = [216, 224, 227, 256, 310, 12000] # from https://codes.wmo.int/wmdr
    # 216 = Atmospheric pressure
    # 224 = Air temperature
    # 227 = Temperature profile
    # 310 = Upper wind
    # 12000 = Upper-air pressure profile

    # Confusing set of networks, discussion on the OSCAR site suggests all synop stations will be GOS but it's not clear
    # how to identify upper-air stations

    networks = ['GOS']
    # GOS - Global Observing System 
    # RBSN - GOS Regional Basic Synoptic Networks 
    # RBSNp - Regional Basic Synoptic Network upper-air station (PILOT) 
    # RBSNs - Regional Basic Synoptic Network surface station (SYNOP) 
    # RBSPst - Regional Basic Synoptic Network surface and upper-air station... 
    # RBSNt - Regional Basic Synoptic Network upper-air station (TEMP) 
    # ANTON - Antarctic Observing Network surface synoptic station (SYNOP/CLIMAT) 
    # ANTONt - Antarctic Observing Network upper-air station (TEMP) 
    statuses = ['operational','preOperational','partlyOperational']

    # search for UK stations to keep the result set smaller for testing
    arg = "territoryName=GBR&territoryName=landFixed&ProgramAffiliation={affiliation}&ObservedVariable={variable}"
    args = arg.format( affiliation=",".join(networks) , variable=",".join(  [str(v) for v in variables])  )
    r = requests.get(SEARCH_API + args)
    if r.status_code != 200:
        print("ERROR: status: {}".format(r.status_code))
        sys.exit(1)

    # This gives us a list of station summaries in json format    
    result = r.json()
    print("we have {} results".format(len(result)))

    # Extract primary wigos id from result, if there is a primary wigos id

    stations = {}  # dict indexed on wigos ID containing details from the search API (but not pressure sensor height!)
    for stn in result:
        print(stn)
        if not stn["declaredStatus"].lower() in statuses: #skip, since not operational
            print("skipping {name} {declaredStatus}".format(**stn))
            continue
        wigosStationIdentifiers = stn.get('wigosStationIdentifiers')  # use get in case the key is missing
        wmo_id = None
        if wigosStationIdentifiers:     # May be more than one
            for item in wigosStationIdentifiers:
                wid = item.get('wigosStationIdentifier')
                m = re.match('0-2000([0-1])-0-(.*)',wid)
        
                if m:
                    wmo_id = m.group(2)
                    sub_index = m.group(1)
                    stn['wmo_id'] = wmo_id
                    stn['sub_index'] = sub_index
                    stations[wid] = stn
                    break
        if not wmo_id:
            print('WMO ID not found')
            continue
    return stations

def process_stations(stations): 
    '''Fetch metadata for each station to complete records.

    Args:
        * stations (dict) - keyed on WIGOS ID

    Returns:
        * results (list of lists) - station details needed for abbreviated list
    '''

    results = []
    count=0

    # Use the retrieval API to get the full metadata record in XML
    for wid, station in stations.items():

        print("checking station {name} id: {id}".format(**station))
         
        # we can get basic elements from the JSON
        lat = '{:9.3f}'.format(station['latitude']) if 'latitude' in station else ''
        lon = '{:9.3f}'.format(station['longitude']) if 'longitude' in station else ''
        station_ht = '{:.0f}'.format(int(station['elevation'])) if 'elevation' in station else ''
        name = station['name']
        wmo_id = station['wmo_id']

        internal_id = station['id']   # for reference - not used
        stn_type =  convert_to_type(station)
        
        url = XML_DOWNLOAD_URL + wid
        print("downloading: ",url)
        res = requests.get(url)
  
        if res.status_code == 200:
            myxml = res.content
            # pressure sensor height from the XML download
            try:
                root = etree.fromstring( myxml )
                sensor_ht = extractPressureInfo(root)
            except etree.XMLSyntaxError as xmle:
                print("parsing error in {} xml: {} error: {}".format(station["name"], myxml[0:200]  , xmle))
            
        
        else:
            print("WARNING: failed to download {wid} {internal_id}".format(wid, internal_id))
            sensor_ht = ''
        
        results.append((wmo_id, lat, lon, sensor_ht, station_ht, stn_type, name))
        count += 1
        if count > 5:
            break
        
    print('results:', len(results))
    # print(results)
    return results


def write_abrv_stnmas(stations, fileout):
    '''Write data in abrv format.

    Args:
       * stations (list of list) - station data, each row ordered

    returns:
       * None
    '''

    line1 = '  WMO      LAT      LONG   SENSOR   STN   STN            STATION\n'
    line2 = ' INDEX   DEG/THS  DEG/THS  HT (M)  HT (M) TYPE           NAME\n'

    # Open output file and write header lines
    with open(fileout, 'w') as f:
        f.write(line1)
        f.write(line2)
        for row in sorted(stations):
            f.write(" {:5}{:9}{:9} {:8}{:8} {:<15}{:<48}\n".
                    format(*row))
        f.write('99999')
    return

if __name__ == "__main__":
    json_data = search_oscar()
    station_data = process_stations(json_data)
    write_abrv_stnmas(station_data, "abrv_stnmas_new")