'''
Module contains functions for use with the MetDB file download API.

See https://metnet2/content/metdb-technote-3b-metdb-web-interface-file-retrieval
for API documentation.

The main benefit to this module is in parsing the XML response format into
a dictionary that looks the same as a JSON response - JSON being easy to read
in python.

Unittests in test_webapi.py demonstrate a range of request formats for
different datatypes.

   >>> import webapi
   >>> url = ('http://mdbdb-prod//cgi-bin/moods/webret.pl?'
   ...        'subtype=SUNPHOTO&'
   ...        'request=INSTR_ID 846 '
   ...        'START TIME 20160323/0000Z '
   ...        'END TIME 20160323/1405Z&'
   ...        'render=json')
   >>> (response, status) = webapi.moods_request(url)
   >>> print(status)
   0
   >>> print(response["moods_obs"])
           1
   >>> webapi.download(response, './', 1)
   Downloading  http://mdbdb-prod/moods/download//CIMEL_AODto...
'''
import os
import sys
import json
import shutil
import urllib.request, urllib.error, urllib.parse
from xml.dom.minidom import parse


def encode_url(url):
    '''Helper function to encode whitespace in URL query

    - Argument - string URL

    - Returns - string URL with whitespace encoded %20
    '''
    parts = urllib.parse.urlsplit(url)
    encoded_query = urllib.parse.quote(parts.query, safe='/=&%')
    new_parts = list(parts[:])
    new_parts[3] = encoded_query
    return urllib.parse.urlunsplit(tuple(new_parts))


def download(response_dict, savepath, verbosity=0):
    '''Issues HTTP requests to download all files to savepath

    - Argument - response_dict - moods response
               - savepath - string location for downloaded files
               - verbosity - int 0 - no output,
                                 1 - print download list

    - Returns - int 0 - success, 1 - I/O error, 2 - URL error
    '''
    status = 0
    for file_details in response_dict['moods_return']:
        url = file_details['url']
        filename = file_details['filename']
        filepath = savepath + '/' + filename
        if verbosity == 1:
            print('Downloading ', url, ' to ', filepath)
        try:
            outfile = open(filepath, 'wb')
            with urllib.request.urlopen(url) as response:
                shutil.copyfileobj(response, outfile)
                outfile.close()
        except (IOError, PermissionError) as err:
            print('File error:', err)
            status = 1
        except urllib.error.URLError as e:
            print('URLError', e.reason)
            status = 2

    return status


def moods_request(url, verbosity=0):
    '''Issue MetDB HTTP request and decode the response

    Argument - url - string containing complete MetDB request
             - verbosity - int 0 - only error output,
                               1 - print URL and response text

    Returns - tuple containing dictionary and status
    '''
    url = encode_url(url)
    if verbosity == 1:
        print('url =  ', url)
    status = 0
    moods_response = None
    try:
        response = urllib.request.urlopen(url)
        if 'xml' in url:
            moods_response = parse_xml(response, verbosity=verbosity)
        elif 'json' in url:
            moods_response = parse_json(response, verbosity=verbosity)
        else:
            print('Format not recognised:', url)
            status = 1

    except urllib.error.HTTPError as err:
        print('HTTPError: ', err.code, err.msg)
        status = err.code
    except urllib.error.URLError as err:
        print('URLError: ', err.reason)
        status = 503
    return (moods_response, status)


def parse_json(response, verbosity=0):
    '''Converts json response text to a dictionary

    Argument - response - string returned by HTTP
             - verbosity - int 0 - no output,
                               1 - print json response

    Returns - json_data - dictionary of response (see parse_xml)
    '''

    text = response.read().decode('utf-8')
    json_data = json.loads(text)
    if verbosity == 1:
        print(json.dumps(json_data, indent=2))

    return json_data


def parse_xml(response, verbosity=0):

    '''Create dictionary from moods response.
       xml_data = {'moods_subtype': subtype ,
                   'moods_request': request string ,
                   'moods_obs': number of obs returned ,
                   'moods_return': list of dictionaries containing
                                   file details
                                   [ {'filename': ...',
                                      'url': ... ,
                                      'filesize':...}, ],
                   'moods_response': dictionary containing status details
                                      {'status': 0 for success, 1 for failure,
                                      'status_text': error message}}
    '''

    xml_data = {}
    toplist = ['moods_subtype', 'moods_request', 'moods_obs']
    returnlist = ['filename', 'url', 'filesize']
    responselist = ['status', 'status_text']

    dataDom = parse(response)
    if verbosity == 1:
        print(dataDom.toprettyxml())

    for node in toplist:
        for n in dataDom.getElementsByTagName(node):
            xml_data[node] = n.firstChild.nodeValue

    moods_return = []
    for returnode in dataDom.getElementsByTagName('moods_return'):
        file_details = {}
        for node in returnlist:
            for n in returnode.getElementsByTagName(node):
                file_details[node] = n.firstChild.nodeValue
        moods_return.append(file_details)

    xml_data['moods_return'] = moods_return

    response_details = {}
    for responsenode in dataDom.getElementsByTagName('moods_response'):
        for node in responselist:
            for n in responsenode.getElementsByTagName(node):
                response_details[node] = n.firstChild.nodeValue

    xml_data['moods_response'] = response_details

    return xml_data


if __name__ == "__main__":
    '''webapi - test program'''

    url = ('http://mdbdb-dummy/cgi-bin/moods/webret.pl?'
           'subtype=SML2NRT&startDateTime=2019-08-06T00:00:00Z&'
           'endDateTime=2019-08-06T04:00:00Z&render=xml')
    (response_dict, status) = moods_request(url, verbosity=2)
    if status == 0:
        print(json.dumps(response_dict, indent=2))
    else:
        print('HTTP error', status)
