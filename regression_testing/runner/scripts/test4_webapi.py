'''Check for invalid request string'''
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
        "END TIME 20140724/1405Z&"
        "render=json")

(response_dict, status) = webapi.moods_request(url)

assert status == 400
print(url.replace(SERVER, 'server'))
print('Passed')
