import unittest
import station_search


class Test_loader(unittest.TestCase):

    def test_valid_icao(self):

        URL = ('http://mdbdb-prod/cgi-bin/moods/printfile.pl?'
               'fullname=/usr/local/moods/tables/icao_list')
        stations = station_search.Stations()
        text = stations.loadData(URL)
        self.assertTrue(len(text) > 0)

    def test_valid_wmo(self):

        URL = ('http://mdbdb-prod/cgi-bin/moods/printfile.pl?'
               'fullname=/var/moods/data/STATIONS/abrv_stnlist')
        stations = station_search.Stations()
        text = stations.loadData(URL)
        self.assertTrue(len(text) > 0)

    def test_invalid_url(self):

        URL = ('http://mdbdb-prod/cgi-bin/moods/printfile.pl?'
               'fullname=/var/moods/data/STATIONS/dummy')
        stations = station_search.Stations()
        self.assertRaises(Exception, stations.loadData(URL))


class Test_lookup_icao(unittest.TestCase):

    def setUp(self):
        self.stnlist = station_search.Stations()
        self.stnlist.getIcao()

    def test_single_icao(self):
        result = self.stnlist.lookup_icao('EGDL')
        self.assertTrue(len(result), 1)
        details = result[0]
        self.assertEqual(details[0], 'EGDL')
        self.assertEqual(details[1], '03740')
        self.assertEqual(details[6], 'LYNEHAM')

    def test_list_icao(self):
        result = self.stnlist.lookup_icao('EGA')
        self.assertTrue(len(result), 5)
        details = result[0]
        self.assertEqual(details[0], 'EGAA')


class Test_lookup_wmo(unittest.TestCase):

    def setUp(self):
        self.stnlist = station_search.Stations()
        self.stnlist.getWmo()

    def test_single_wmo(self):
        result = self.stnlist.lookup_wmo('03772')
        self.assertTrue(len(result), 1)
        details = result[0]
        self.assertEqual(details[0], '')
        self.assertEqual(details[1], '03772')
        self.assertEqual(details[6], 'HEATHROW')

    def test_list_wmo(self):
        result = self.stnlist.lookup_wmo('037')
        self.assertTrue(len(result), 16)
        details = result[0]
        self.assertEqual(details[0], '')
        self.assertEqual(details[1], '03705')
        self.assertEqual(details[6], 'SAUNTON SANDS')


class Test_lookup_both(unittest.TestCase):

    def setUp(self):
        self.stnlist = station_search.Stations()
        self.stnlist.getIcao()
        self.stnlist.getWmo()

    def test_name(self):
        result = self.stnlist.lookup_wmo_name('HEATH')
        self.assertTrue(len(result), 5)
        result = self.stnlist.lookup_icao_name('HEATH')
        self.assertTrue(len(result), 4)
