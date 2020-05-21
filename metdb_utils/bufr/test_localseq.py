import localseq
import unittest
import re
import os
from io import StringIO
from unittest.mock import patch


class MockDevice():
    """A mock device to temporarily suppress output to stdout
    Similar to UNIX /dev/null.
    http://keenhenry.me/suppress-stdout-in-unittest
    """

    def write(self, s):
        pass


class Test_localseq(unittest.TestCase):

    def setUp(self):
        os.environ.update({'BUFR_LIBRARY':
                           '/home/moodsf/MetDB_BUFR24.0.00/tables/'})

    def test_no_file(self):
        ''' No input file                             '''

        with patch('sys.stdout', new=StringIO()) as out:
            with self.assertRaises(SystemExit) as cm:
                s = localseq.read_localseq('')

        self.assertEqual(cm.exception.code, 1)

    def test_aatsr(self):
        '''' Valid input file                         '''

        with patch('sys.stdout', new=StringIO()) as out:
            s = localseq.read_localseq('test_data/aatsr')

        # File contains 3 sequences
        self.assertEqual(len(s), 3)

        # Check all sequences have been found
        self.assertIn('312231', s)
        self.assertIn('312045', s)
        self.assertIn('312230', s)

        # Check the length of one of them
        self.assertEqual(len(s['312231']), 23)

    def test_tropcycl(self):
        '''' Input with descriptors in comments       '''

        with patch('sys.stdout', new=StringIO()) as out:
            s = localseq.read_localseq('test_data/tropcycl')

        # File contains 4 sequences
        self.assertEqual(len(s), 4)

        # Check the length of one containing descriptors in 
        # comments
        self.assertEqual(len(s['316200']), 41)

    def test_merge_ascat(self):
        '''' Input with no comments after single descriptors    '''

        with patch('sys.stdout', new=StringIO()) as out:
            s = localseq.read_localseq('test_data/ascat_merge')

        # File contains 1 sequence
        self.assertEqual(len(s), 1)

        # Check the length 
        self.assertEqual(len(s['312210']), 120)

    def test_merge_atovs(self):
        '''' Complicated input!    '''

        with patch('sys.stdout', new=StringIO()) as out:
            s = localseq.read_localseq('test_data/atovs_merge')

        # File contains 3 sequences
        self.assertEqual(len(s), 3)

        # Check the length of the last one
        self.assertEqual(len(s['310224']), 232)

if __name__ == '__main__':
    unittest.main()
