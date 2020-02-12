'''Test valid LIDAR request with REQUEST string'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = "http://" + SERVER + "/cgi-bin/moods/webret.pl?"
url += ("subtype=LIDAR&"
        "request=START TIME 20190806/0000Z "
        "END TIME 20190806/0200Z "
        "PLATFORM wattisham "
        "SELECT nimbus &"
        "render=json")

(response_dict, status) = webapi.moods_request(url)

assert status == 0
nobs = int(response_dict['moods_obs'])
assert nobs == 9
print(url.replace(SERVER, 'server'))
print('Passed')
