import bufr
import os
import unittest
from unittest.mock import patch

class MockDevice():
    """A mock device to temporarily suppress output to stdout
    Similar to UNIX /dev/null.
    http://keenhenry.me/suppress-stdout-in-unittest
    """

    def write(self, s): pass


class Test_bufrD(unittest.TestCase):

    def setUp(self):
        os.environ.update({'BUFR_LIBRARY': '/home/moodsf/MetDB_BUFR24.0.00/tables/'})

    def test_bufrD(self):
        with patch('sys.stdout', new=MockDevice()) as fake_out:
            tableD = bufr.TableD()

        self.assertEqual(tableD.lookup('300002'),['000002', '000003'])
        self.assertEqual(tableD.lookup('301006'), None)
        self.assertEqual(len(tableD.lookup('301058')), 37)

    def test_bufrB(self):
        with patch('sys.stdout', new=MockDevice()) as fake_out:
            tableB = bufr.TableB()

        self.assertEqual(tableB.lookup('001007').name,'SATELLITE IDENTIFIER')
        self.assertEqual(tableB.lookup('001088'), None)

    def test_long_bufrD(self):
        with patch('sys.stdout', new=MockDevice()) as fake_out:
            tableD = bufr.TableD()

        seq = tableD.lookup('310077')
        self.assertEqual(len(seq), 127)

    def test_expansion(self):
        with patch('sys.stdout', new=MockDevice()) as fake_out:
            tableD = bufr.TableD()

            self.assertEqual(tableD.expander(['001001']),['001001'])
            self.assertEqual(tableD.expander(['001001', '001002']),
                                             ['001001', '001002'])
            self.assertEqual(tableD.expander(['300003']),
                                             ['000010', '000011', '000012'])
            self.assertEqual(tableD.expander(['300004']),
                                             ['000010', '000011', '000012', '000013',
                                              '000014', '000015', '000016', '000017',
                                              '000018', '000019', '000020'])
    def test_fxys(self):
        self.assertEqual(bufr.fxy(263), '001007')
        self.assertEqual(bufr.fxy(2581), '010021')
        self.assertEqual(bufr.fxy(16643), '101003')
        self.assertEqual(bufr.fxy(37888), '220000')
        self.assertEqual(bufr.fxy(49411), '301003')

class Test_is_a_descriptor(unittest.TestCase):

    def test_valid(self):
        '''All valid descriptors'''
        self.assertTrue(bufr.is_a_descriptor('015012'))
        self.assertTrue(bufr.is_a_descriptor('112232'))
        self.assertTrue(bufr.is_a_descriptor('212412'))
        self.assertTrue(bufr.is_a_descriptor('323012'))

    def test_empty(self):
        '''Empty string'''
        self.assertFalse(bufr.is_a_descriptor(''))

    def test_type(self):
        '''Non-string inputs'''
        self.assertFalse(bufr.is_a_descriptor(312212))
        self.assertFalse(bufr.is_a_descriptor(True))

    def test_alpha(self):
        '''Not digits'''
        self.assertFalse(bufr.is_a_descriptor('rubbish'))
        self.assertFalse(bufr.is_a_descriptor('$@#'))

    def test_decimal(self):
        '''Wrong sort of numbers'''
        self.assertFalse(bufr.is_a_descriptor('12.234'))
        self.assertFalse(bufr.is_a_descriptor('-12312'))

    def test_length(self):
        '''Invalid length'''
        self.assertFalse(bufr.is_a_descriptor('11'))
        self.assertFalse(bufr.is_a_descriptor('1234567'))

    def test_range(self):
        '''Descriptors must start with 0,1,2 or 3'''
        self.assertFalse(bufr.is_a_descriptor('401001'))



if __name__ == '__main__':
  unittest.main()
