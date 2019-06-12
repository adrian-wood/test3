import bufr
import unittest
from unittest.mock import patch

class MockDevice():
    """A mock device to temporarily suppress output to stdout
    Similar to UNIX /dev/null.
    http://keenhenry.me/suppress-stdout-in-unittest
    """

    def write(self, s): pass

class Test_bufrD(unittest.TestCase):
    def test_cases(self):
        with patch('sys.stdout', new=MockDevice()) as fake_out:
            tableD = bufr.TableD()

        self.assertEqual(tableD.lookup('300002'),['000002', '000003'])
        self.assertEqual(tableD.lookup('301006'), None)
        self.assertEqual(len(tableD.lookup('301058')), 37)

class Test_bufrB(unittest.TestCase):
    def test_cases(self):
        with patch('sys.stdout', new=MockDevice()) as fake_out:
            tableB = bufr.TableB()

        self.assertEqual(tableB.lookup('001007').name,'SATELLITE IDENTIFIER')
        self.assertEqual(tableB.lookup('001088'), None)

class Test_expansion(unittest.TestCase):
    def test_cases(self):
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
class Test_fxy(unittest.TestCase):
    def test_cases(self):
        self.assertEqual(bufr.fxy(263), '001007')
        self.assertEqual(bufr.fxy(2581), '010021')
        self.assertEqual(bufr.fxy(16643), '101003')
        self.assertEqual(bufr.fxy(37888), '220000')
        self.assertEqual(bufr.fxy(49411), '301003')


if __name__ == '__main__':
  unittest.main()
