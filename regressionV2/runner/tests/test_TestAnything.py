import unittest
import sys
sys.path.append("../scripts/")
import TestAnything as ta

class Test_plan(unittest.TestCase):


    def test_no_plan(self):

        plan = ta.TestPlan()

        with self.assertRaises(SystemExit) as e:
            plan.readPlan('dummy.txt')

        self.assertEqual(e.exception.code, 1)

    def test_read_testplan(self):

        plan = ta.TestPlan()
        plan.readPlan('/var/moods/regression/TestPlan.txt')

        self.assertEqual(plan.count, 72)
        self.assertEqual(plan.success, 0)
        self.assertEqual(plan.fail, 0)
       
       
    def test_missing_colon_before_number(self):

        plan = ta.TestPlan()
        with self.assertRaises(SystemExit) as e:
            plan.readPlan('BadPlan1.txt')
        self.assertEqual(e.exception.code, 1)

    def test_not_enough_columns(self):

        plan = ta.TestPlan()
        with self.assertRaises(SystemExit) as e:
            plan.readPlan('BadPlan2.txt')
        self.assertEqual(e.exception.code, 1)

    def test_read_test_results_with_no_plan(self):

        plan = ta.TestPlan()
        with self.assertRaises(SystemExit) as e:
            plan.readTAP('/var/moods/regression/TestResults.txt')
        self.assertEqual(e.exception.code, 1)

    def test_plan_and_results_not_matching(self):

        plan = ta.TestPlan()
        plan.readPlan('/var/moods/regression/TestPlan.txt')
        with self.assertRaises(SystemExit) as e:
            plan.readTAP('BadResults1.txt')
        self.assertEqual(e.exception.code, 1)

         
if __name__ == '__main__':
    unittest.main()
