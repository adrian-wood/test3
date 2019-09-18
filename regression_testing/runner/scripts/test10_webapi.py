'''Test LIDAR platform with underscore'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = "http://" + SERVER + "/cgi-bin/moods/webret.pl?"
url += ("subtype=LIDAR&"
        "request=PLATFORM ST_HELENA "
        "START TIME 20190807/2305Z "
        "END TIME 20190809/0005Z "
        "SELECT ct25&"
        "render=json")

(response_dict, status) = webapi.moods_request(url)

assert status == 0
nobs = int(response_dict['moods_obs'])
assert nobs == 26
print(url.replace(SERVER, 'server'))
print('Passed')
