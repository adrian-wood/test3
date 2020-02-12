'''Check for invalid subtype'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = "http://" + SERVER + "/cgi-bin/moods/webret.pl?"
url += ("subtype=DUMMY&"
        "startDateTime=2019-08-06T00:00:00Z&"
        "endDateTime=2019-08-06T04:00:00Z&"
        "render=xml")

(response_dict, status) = webapi.moods_request(url)

assert status == 0
moods_status = response_dict['moods_response']['status']
assert int(moods_status) == 1
print(url.replace(SERVER, 'server'))
print('Passed')
