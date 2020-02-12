'''Check for 503 HTTP return code if server not found'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = ("http://mdbdb-dummy/cgi-bin/moods/webret.pl?"
       "subtype=SML2NRT&"
       "startDateTime=2019-08-06T00:00:00Z&"
       "endDateTime=2019-08-06T04:00:00Z&"
       "render=xml")

(response_dict, status) = webapi.moods_request(url)
assert status == 503
print(url.replace(SERVER, 'server'))
print('Passed')
