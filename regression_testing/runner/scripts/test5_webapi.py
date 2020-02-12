'''Test valid SUNPHOTO request'''
import webapi
import os
import sys

if len(sys.argv) > 1:
    SERVER = sys.argv.pop()
else:
    SERVER = 'mdb-test'

url = "http://" + SERVER + "/cgi-bin/moods/webret.pl?"
url += ("subtype=SUNPHOTO&"
        "request=START TIME 20160323/0000Z "
        "END TIME 20160323/1405Z&"
        "render=json")

(response_dict, status) = webapi.moods_request(url)

assert status == 0
nobs = int(response_dict['moods_obs'])
assert nobs == 1
print(url.replace(SERVER, 'server'))
print('Passed')
