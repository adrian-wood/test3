import unittest
import numpy as np
import get_data
import unit_utils
from datetime import datetime

MDI = np.ma.masked


class Test_time_from_ref(unittest.TestCase):
    def test_cases(self):

        # time_from_ref takes 1 datetime object and any combination of:
        # hour: no default
        # start: default True
        # days_ago: default 0 (days ago i.e. today)
        # time_span: default 1 (hour)
        ref = datetime.strptime('Jan 7 2019  1:33PM', '%b %d %Y %I:%M%p')

        # A simple case with no optional arguments...
        result = get_data.time_from_ref(ref)
        self.assertEqual(result, '20190107/1333Z')

        # Various combinations of optional arguments...
        cases = [
          ({'hour': 8},                                                  '20190107/0800Z'),
          ({'hour': 19},                                                 '20190106/1900Z'),
          ({'hour': 8,  'start': False},                                 '20190107/0859Z'),
          ({'hour': 19, 'start': False},                                 '20190106/1959Z'),
          ({'hour': 3,  'days_ago': 1},                                  '20190106/0300Z'),
          ({'hour': 20, 'days_ago': 1},                                  '20190106/2000Z'),
          ({'hour': 11, 'days_ago': 2},                                  '20190105/1100Z'),
          ({'hour': 11, 'days_ago': 2, 'start': False},                  '20190105/1159Z'),
          ({'hour': 11, 'days_ago': 4, 'start': True,  'time_span': 2},  '20190103/1100Z'),
          ({'hour': 11, 'days_ago': 4, 'start': False, 'time_span': 2},  '20190103/1259Z'),
          ({'hour': 0,  'days_ago': 1},                                  '20190106/0000Z'),
          ({'hour': 0,  'days_ago': 1, 'start': False, 'time_span': 24}, '20190106/2359Z'),
          ({'hour': 6,  'days_ago': 1, 'start': False, 'time_span': 24}, '20190107/0559Z'),
          ({'hour': 18, 'days_ago': 1, 'start': False, 'time_span': 24}, '20190107/1759Z'),
          ({'hour': 18, 'start': False, 'time_span': 24},                '20190107/1759Z'),
        ]

        for args, expected in cases:
            result = get_data.time_from_ref(ref, **args)
            self.assertEqual(expected, result)


class Test_code_lookup(unittest.TestCase):
    def test_cases(self):

        # normal case
        self.assertEqual(unit_utils.code_lookup("020192", 1), "1 LIGHT")
        # code table has no text
        self.assertEqual(unit_utils.code_lookup("020192", 4), "4")
        # normal case
        self.assertEqual(unit_utils.code_lookup("020192", 5), "5 IN VICINITY")
        # code table doesn't contain value
        self.assertEqual(unit_utils.code_lookup("020192", 6), "6")
        # missing data in value
        self.assertEqual(unit_utils.code_lookup("020192", MDI), "")
        # table not found
        self.assertEqual(unit_utils.code_lookup("020199", 1), "1")


class Test_flag_lookup(unittest.TestCase):
    def test_cases(self):

        # missing data
        self.assertEqual(unit_utils.flag_lookup("020193", MDI), "")
        # single flag, undefined
        self.assertEqual(unit_utils.flag_lookup("020193", 0), "")
        # single flag, defined
        self.assertEqual(unit_utils.flag_lookup("020193", 128), "1 PARTIAL       PR")
        # single flag, defined
        self.assertEqual(unit_utils.flag_lookup("020193", 64), "2 SUPERCOOLED   FZ")
        # two consecutive flags
        self.assertEqual(unit_utils.flag_lookup("020193", 192),
                         "2 SUPERCOOLED   FZ;1 PARTIAL       PR")
        # flag at other end of table
        self.assertEqual(unit_utils.flag_lookup("020193", 1), "8 SHALLOW       MI")
        # two flags at either end of table
        self.assertEqual(unit_utils.flag_lookup("020193", 129),
                         "8 SHALLOW       MI;1 PARTIAL       PR")
        # table not found (can't do anything without bitwidth)
        with self.assertRaises(SystemExit) as cm:
            unit_utils.flag_lookup("020199", 1)
        self.assertEqual(cm.exception.code, 2)


if __name__ == '__main__':
    unittest.main()
