'''Test valid LIDAR request with parms'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = "http://" + SERVER + "/cgi-bin/moods/webret.pl?"
url += ("subtype=LIDAR&"
        "startDateTime=2019-07-23T00:00:00+0000&"
        "endDateTime=2019-07-23T23:59:00+0000&"
        "platform=SHANNON&"
        "select=cl31&"
        "render=xml")

(response_dict, status) = webapi.moods_request(url)

assert status == 0
nobs = int(response_dict['moods_obs'])
assert nobs == 24
print(url.replace(SERVER, 'server'))
print('Passed')
