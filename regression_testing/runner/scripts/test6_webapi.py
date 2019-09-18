'''Test valid VA_LIDAR request'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = "http://" + SERVER + "/cgi-bin/moods/webret.pl?"
url += ("subtype=VA_LIDAR&"
        "request=START TIME 20190724/0000Z "
        "END TIME 20190724/1405Z&"
        "render=json")

(response_dict, status) = webapi.moods_request(url)

assert status == 0
nobs = int(response_dict['moods_obs'])
assert nobs == 4
print(url.replace(SERVER, 'server'))
print('Passed')
