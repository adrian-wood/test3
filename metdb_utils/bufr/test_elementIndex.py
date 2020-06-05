import elementIndex as ec
import unittest
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
            table = ec.read_elements('')

        self.assertIn('FileNotFound', out.getvalue())
        self.assertIsNone(table)

    def test_valid_elements(self):
        '''aatsr elements index                 '''

        filename = 'test_data/aatsr_elements'

        table = ec.read_elements(filename)
        
        self.assertIsNotNone(table)
        lseq = table.list_sequences()
        self.assertEqual(len(lseq), 4)
        lnames = table.list_element_names()
        self.assertEqual(len(lnames), 31)

    def test_long_elements(self):
        '''synopt elements index                 '''

        filename = 'test_data/synopt_elements'

        table = ec.read_elements(filename)
        
        self.assertIsNotNone(table)
        lseq = table.list_sequences()
        self.assertEqual(len(lseq), 24)
        lnames = table.list_element_names()
        self.assertEqual(len(lnames), 203)
        # check an index with replication counts that span more
        # than one line
        nreps = table.indexes[2].nrep
        self.assertEqual(nreps, 27)


if __name__ == '__main__':
    unittest.main()
