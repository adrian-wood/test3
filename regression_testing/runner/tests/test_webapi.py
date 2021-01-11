import unittest
import sys
from io import StringIO
from unittest.mock import patch
sys.path.append("../scripts/")
import webapi

class Test_webapi(unittest.TestCase):


    def test_response(self):

        url = ('http://mdbdb-prod//cgi-bin/moods/webret.pl?subtype=SUNPHOTO&'
               'request=INSTR_ID 846 START TIME 20160323/0000Z '
               'END TIME 20160323/1405Z&render=json')

        (response, status) = webapi.moods_request(url)

        # successful request
        self.assertEqual(status, 0)

        # one file found
        self.assertEqual(int(response["moods_obs"]), 1)

        # Correct file found
        moods_return = response["moods_return"]
        self.assertEqual(len(moods_return), 1)
        first_file = moods_return[0]
        self.assertEqual(first_file["filesize"], "                  49 KB")
        self.assertEqual(first_file["filename"],
                         "CIMEL_AODtotal_sensor846_20160323T111428-20160323T115420.nc")

    def test_download(self):
 
        url = ('http://mdbdb-prod//cgi-bin/moods/webret.pl?subtype=SUNPHOTO&'
               'request=INSTR_ID 846 START TIME 20160323/0000Z '
               'END TIME 20160323/1405Z&render=json')

        (response, status) = webapi.moods_request(url)

        with patch('sys.stdout', new=StringIO()) as out:
            status = webapi.download(response, '/tmp/', 1)

        lines = out.getvalue().split('\n')
        self.assertIn('Downloading  http://mdbdb-prod/moods/download//CIMEL_AODtotal', lines[0])
        self.assertEqual(status, 0)


if __name__ == '__main__':
    unittest.main()
