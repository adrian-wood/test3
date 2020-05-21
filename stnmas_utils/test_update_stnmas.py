import unittest
import pandas as pd
import update_stnmas as up



class Test_convert_to_decimal(unittest.TestCase):

    def test_valid(self):

       self.assertEqual(up.convert_to_decimal(' 36 49 00N'), '   36.817')
       self.assertEqual(up.convert_to_decimal('000 17 00E'), '    0.283')
       self.assertEqual(up.convert_to_decimal('082 54 08W'), '  -82.902')
       self.assertEqual(up.convert_to_decimal('66 43 00S'),  '  -66.717')
       self.assertEqual(up.convert_to_decimal('179 03 01W'), ' -179.050')

    def test_invalid(self):

        with self.assertRaises(SystemExit) as cm:
            up.convert_to_decimal('')
        self.assertEqual(cm.exception.code, 8)

        with self.assertRaises(SystemExit) as cm:
            up.convert_to_decimal('00 N')
        self.assertEqual(cm.exception.code, 8)


class Test_convert_to_type(unittest.TestCase):

    def test_valid(self):
        row_dict =  { 'ObsRems' : 'Climatological station, Surface land meteorological station (SYNOP), RBCN, RBSN(S), GOS, GSN, CLIMAT(C)', 
                      'IndexSubNbr' : 0.0}

        row = pd.Series(row_dict)
        self.assertEqual(up.convert_to_type(row), 'SURF')

        row_dict =  {'ObsRems' : 'Upper-air / Radiosonde station, RBSN(T), GOS, GUAN',
                     'IndexSubNbr' : 0.0}
        row = pd.Series(row_dict)
        self.assertEqual(up.convert_to_type(row), 'SURF & U/AIR')

        row_dict =  {'ObsRems' : 'Upper-air / Radiosonde station, RBSN(T), GUAN',
                     'IndexSubNbr' : 0.0}
        row = pd.Series(row_dict)
        self.assertEqual(up.convert_to_type(row), 'U/AIR')

        row_dict =  {'ObsRems' : 'Climatological station, Surface land meteorological station (SYNOP), RBCN, RBSN(S), GOS, GSN',
                     'IndexSubNbr' : 1.0}
        row = pd.Series(row_dict)
        self.assertEqual(up.convert_to_type(row), 'U/AIR')

        row_dict =  {'ObsRems' : 'Climatological station, Surface land meteorological station (SYNOP), RBCN, RBSN(S), GOS, GSN',
                     'IndexSubNbr' : 0.0}
        row = pd.Series(row_dict)
        self.assertEqual(up.convert_to_type(row), 'SURF')

class Test_read_stnmas(unittest.TestCase):

    def test_valid(self):

        stnmas = up.read_stnmas('test_data/abrv_stnlist')
        self.assertIsInstance(stnmas, pd.DataFrame)
        self.assertEqual(stnmas.shape, (41, 7))

    def test_invalid(self):

        self.assertEqual(up.read_stnmas('test_data/dummy'), None)


class Test_read_vola(unittest.TestCase):

    def test_valid(self):

        vola = up.read_vola('test_data/vola_one')
        self.assertIsInstance(vola, pd.DataFrame)
        self.assertEqual(vola.shape, (28, 7))

    
    def test_invalid(self):

        self.assertEqual(up.read_vola('test_data/dummy'), None)



   