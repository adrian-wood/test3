import unittest
# import sys
# sys.path.append('../..')
import RetrievalTable as RT
import DataAccessLog as DA


class Test_Retrieval_Table(unittest.TestCase):

    def setUp(self):
        with open('test_data/retrieval_table') as f:
            self.rt = RT.RetrievalTable(f)

    def test_count_datatypes(self):
        self.assertEqual(len(self.rt.list_datatypes()), 10)

    def test_list_datatypes(self):
        self.assertEqual(sorted(self.rt.list_datatypes()),
                         sorted(['AATSR', 'JASON2', 'LIDAR', 'LNDSYN',
                                 'METARS', 'SHPSYN', 'TAFS', 'SUNPHOTO',
                                 'TEMP', 'VA_LIDAR']))

    def test_count_datasets(self):
        self.assertEqual(self.rt.dataset_count('AATSR'), 1)
        self.assertEqual(self.rt.dataset_count('JASON2'), 1)
        self.assertEqual(self.rt.dataset_count('LIDAR'), 1)
        self.assertEqual(self.rt.dataset_count('LNDSYN'), 10)
        self.assertEqual(self.rt.dataset_count('METARS'), 8)
        self.assertEqual(self.rt.dataset_count('SHPSYN'), 13)
        self.assertEqual(self.rt.dataset_count('TAFS'), 2)
        self.assertEqual(self.rt.dataset_count('SUNPHOTO'), 1)
        self.assertEqual(self.rt.dataset_count('TEMP'), 12)
        self.assertEqual(self.rt.dataset_count('VA_LIDAR'), 1)


class Test_Data_Access_Log(unittest.TestCase):

    def setUp(self):
        with open('test_data/data_access.log', errors='ignore') as f:
            self.da = DA.DataAccessLog(f)

    def test_count_by_datatype(self):
        self.assertEqual(self.da.count_by_datatype['SNOW'], 1)
        self.assertEqual(self.da.count_by_datatype['ESACSR'], 2)
        self.assertEqual(self.da.count_by_datatype['MWRI'], 2)
        self.assertEqual(self.da.count_by_datatype['WOW'], 2)
        self.assertEqual(self.da.count_by_datatype['AMSR2'], 3)
        self.assertEqual(self.da.count_by_datatype['SREW'], 48)
        self.assertEqual(self.da.count_by_datatype['RADRRATE'], 50)
        self.assertEqual(self.da.count_by_datatype['BUOY'], 55)
        self.assertEqual(self.da.count_by_datatype['LNDSYN'], 83)
        self.assertEqual(self.da.count_by_datatype['SHPSYN'], 86)

if __name__ == '__main__':
    unittest.main()
