'''Test LIDAR keyword order'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = "http://" + SERVER + "/cgi-bin/moods/webret.pl?"
url += ("subtype=LIDAR&"
        "request=PLATFORM LERWICK "
        "START TIME 20190808/0000Z "
        "END TIME 20190808/2359Z "
        "SELECT nimbus&"
        "render=xml")

(response_dict, status) = webapi.moods_request(url)

assert status == 0
nobs = int(response_dict['moods_obs'])
assert nobs == 96
print(url.replace(SERVER, 'server'))
print('Passed')
