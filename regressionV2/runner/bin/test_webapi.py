import webapi
import os
import sys
import unittest
from io import StringIO
from unittest.mock import patch


class MockDevice():
    """A mock device to temporarily suppress output to stdout
    Similar to UNIX /dev/null.
    http://keenhenry.me/suppress-stdout-in-unittest
    """

    def write(self, s):
        pass


class Test_URL_API(unittest.TestCase):

    SERVER = 'mdb-test'
    DEBUG = False

    def test_bad_machine(self):
        url = ("http://mdbdb-dummy/cgi-bin/moods/webret.pl?"
               "subtype=SML2NRT&"
               "startDateTime=2019-08-06T00:00:00Z&"
               "endDateTime=2019-08-06T04:00:00Z&"
               "render=xml")

        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 503)

    def test_bad_procedure(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/dummy.pl?"
        url += ("subtype=SML2NRT&"
                "startDateTime=2019-08-06T00:00:00Z&"
                "endDateTime=2019-08-06T04:00:00Z&"
                "render=xml")
        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 404)
        self.assertEqual(out.getvalue().strip(),
                         'HTTPError:  404 Not Found')

    def test_invalid_subtype(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=DUMMY&"
                "startDateTime=2019-08-06T00:00:00Z&"
                "endDateTime=2019-08-06T04:00:00Z&"
                "render=xml")
        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 404)
        self.assertEqual(out.getvalue().strip(),
                         'HTTPError:  404 Not Found')

    def test_invalid_request_string(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=VA_LIDAR&"
                "request=START TIME 20190724/0000Z "
                "END TIME 20140724/1405Z&"
                "render=json")
        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 400)

    def test_sunphoto_valid(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=SUNPHOTO&"
                "request=START TIME 20160323/0000Z "
                "END TIME 20160323/1405Z&"
                "render=json")
        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 0)
        self.assertEqual(int(response_dict['moods_obs']), 1)

    def test_va_lidar_valid(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=VA_LIDAR&"
                "request=START TIME 20190724/0000Z "
                "END TIME 20190724/1405Z&"
                "render=json")
        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 0)
        self.assertEqual(int(response_dict['moods_obs']), 4)

    def test_lidar_valid_parms(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=LIDAR&"
                "startDateTime=2019-07-23T00:00:00+0000&"
                "endDateTime=2019-07-23T23:59:00+0000&"
                "platform=SHANNON&"
                "select=cl31&"
                "render=xml")
        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 0)
        self.assertEqual(int(response_dict['moods_obs']), 24)

    def test_lidar_valid_request(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=LIDAR&"
                "request=START TIME 20190806/0000Z "
                "END TIME 20190806/0200Z "
                "PLATFORM wattisham "
                "SELECT nimbus &"
                "render=json")

        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 0)
        self.assertEqual(int(response_dict['moods_obs']), 9)

    def test_lidar_keyword_order(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=LIDAR&"
                "request=PLATFORM LERWICK "
                "START TIME 20190808/0000Z "
                "END TIME 20190808/2359Z "
                "SELECT nimbus&"
                "render=xml")

        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 0)
        self.assertEqual(int(response_dict['moods_obs']), 96)

    def test_lidar_platform_underscore(self):
        url = "http://" + self.SERVER + "/cgi-bin/moods/webret.pl?"
        url += ("subtype=LIDAR&"
                "request=PLATFORM ST_HELENA "
                "START TIME 20190807/2305Z "
                "END TIME 20190809/0005Z "
                "SELECT ct25&"
                "render=json")

        if self.DEBUG: print(url)
        with patch('sys.stdout', new=StringIO()) as out:
            (response_dict, status) = webapi.moods_request(url)

        self.assertEqual(status, 0)
        self.assertEqual(int(response_dict['moods_obs']), 26)

if __name__ == '__main__':
    if len(sys.argv) > 1:
        Test_URL_API.SERVER = sys.argv.pop()
    Test_URL_API.DEBUG = True 
    unittest.main(verbosity=2)
