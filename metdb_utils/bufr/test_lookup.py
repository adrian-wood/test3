import lookup
import unittest
import re
import os
from io import StringIO
from unittest.mock import patch


class Test_lookup(unittest.TestCase):

    def setUp(self):
        os.environ.update({'BUFR_LIBRARY':
                           '/home/moodsf/MetDB_BUFR24.0.00/tables/'})

    def test_no_file(self):
        ''' No input file                             '''

        with patch('sys.stdout', new=StringIO()) as out:
            lookup.lookup('', 1, 0)

        self.assertIn('ERROR', out.getvalue())

    def test_invalid_indexes(self):
        '''Invalid index and/or sequence              '''

        filename = 'test_data/aatsr_elements'
        with self.assertRaises(KeyError):
            lookup.lookup(filename, 0, 0)
        with self.assertRaises(IndexError):
            lookup.lookup(filename, 1, 4)

    def test_valid_elements(self):
        '''aatsr elements index output                '''

        filename = 'test_data/aatsr_elements'

        with patch('sys.stdout', new=StringIO()) as out:
            lookup.lookup(filename, 1, 0)
        output = out.getvalue().split('\n')
        self.assertEqual(len(output), 157)
        self.assertIn('  [[STLT_IDNY]]  ', output)


if __name__ == '__main__':
    unittest.main()
