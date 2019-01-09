import unittest
import get_data
from datetime import datetime


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
      ({'hour':8},                                               '20190107/0800Z'),
      ({'hour':19},                                              '20190106/1900Z'),
      ({'hour':8,  'start':False},                               '20190107/0859Z'),
      ({'hour':19, 'start':False},                               '20190106/1959Z'),
      ({'hour':3,  'days_ago':1},                                '20190106/0300Z'),
      ({'hour':20, 'days_ago':1},                                '20190106/2000Z'),
      ({'hour':11, 'days_ago':2},                                '20190105/1100Z'),
      ({'hour':11, 'days_ago':2, 'start':False},                 '20190105/1159Z'),
      ({'hour':11, 'days_ago':4, 'start':True,  'time_span':2},  '20190103/1100Z'),
      ({'hour':11, 'days_ago':4, 'start':False, 'time_span':2},  '20190103/1259Z'),
      ({'hour':0,  'days_ago':1},                                '20190106/0000Z'),
      ({'hour':0,  'days_ago':1, 'start':False, 'time_span':24}, '20190106/2359Z'),
    ]

    for args, expected in cases:
      result = get_data.time_from_ref(ref, **args)
      self.assertEqual(expected, result)


if __name__ == '__main__':
  unittest.main()
