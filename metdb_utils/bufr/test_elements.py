import elements
import unittest
import re
import os
from io import StringIO
from unittest.mock import patch


def get_elements(lines):
    ''' Parses the element name lines to return the segment
        and position as lists'''
    s = []
    p = []
    for i in lines:
        c = re.sub(' +', ' ', i)
        c = c.split(' ')
        if len(c) >= 3:
            p.append(c[-1])
            s.append(c[-2])
        else:
            break
    return (s, p)


class MockDevice():
    """A mock device to temporarily suppress output to stdout
    Similar to UNIX /dev/null.
    http://keenhenry.me/suppress-stdout-in-unittest
    """

    def write(self, s):
        pass


class Test_elements(unittest.TestCase):

    def setUp(self):
        os.environ.update({'BUFR_LIBRARY': '/home/moodsf/MetDB_BUFR24.0.00/tables/'})

    def test_simpleB(self):
        ''' Simple sequence, table B only                                '''

        sequence = ['001001', '001002', '004001', '004002', '004003',
                    '004004', '004005', '004006', '005001', '006001',
                    '012001', '012003']

        summary = '1   0   0'
        map1 = '1  12   0'
        seg = ['1'] * 12
        pos = [str(_) for _ in range(1, 13)]

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[9])
        self.assertIn(map1, lines[11])

        (s, p) = get_elements(lines[17:])
        self.assertEqual(s, seg)
        self.assertEqual(p, pos)

    def test_aatsr(self):

        ''' Simple sequence - no replications, 201 operators and table Ds'''

        text = ('001007 002019 001096 025061 005040 301011 301013 301021 '
                '007002 012180 012181 012182 012183 012184 012185 201129 '
                '002174 201000 021086 012186 021087 012187 033043')

        sequence = text.strip().split(' ')
        summary = '1   0   0'
        map1 = '1  26   0'
        seg = ['1'] * 26
        pos = [str(_) for _ in range(1, 27)]

        table =  elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[10])
        self.assertIn(map1, lines[12])

        (s, p) = get_elements(lines[18:])
        self.assertEqual(s, seg)
        self.assertEqual(p, pos)

    def test_airqal(self):
        ''' Simple sequence - no replications, 208 operators and table Ds  '''

        text = ('001019 001214 001215 208008 001216 208000 001217 001218 '
                '001219 301011 301013 301021 007030 007032 008021 004025 '
                '008043 008044 008045 008090 015023 008090 033003')

        sequence = text.strip().split(' ')
        summary = '1   0   0'
        map1 = '1  26   0'
        seg = ['1'] * 26
        pos = [str(_) for _ in range(1, 27)]

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[10])
        self.assertIn(map1, lines[12])

        (s, p) = get_elements(lines[18:])
        self.assertEqual(s, seg)
        self.assertEqual(p, pos)

    def test_airs(self):
        ''' Delayed and fixed replications, no nesting, 201 & 202 operators'''

        text = ('008070 001033 008070 001033 001007 005040 201133 005041 '
                '201000 201132 025070 201000 202126 007001 202000 007025 '
                '005022 002151 012064 002151 012064 002151 012064 002151 '
                '012064 002151 012064 002151 012064 002019 004001 004002 '
                '004003 004004 004005 202131 201138 004006 201000 202000 '
                '005001 006001 007024 005021 005043 107000 031002 201134 '
                '005042 201000 025076 033032 012163 014201 025194 008194 '
                '025195 007002 113004 201134 005042 201000 025076 033032 '
                '201131 202129 008023 014027 008023 014027 202000 201000 '
                '002019 004001 004002 004003 004004 004005 202131 201138 '
                '004006 201000 202000 005001 006001 007024 005021 005043 '
                '105015 005042 025076 033032 012163 014201 025194 008194 '
                '025195 007002 002019 004001 004002 004003 004004 004005 '
                '202131 201138 004006 201000 202000 005001 006001 007024 '
                '005021 005043 105004 005042 025076 033032 012163 014201 '
                '025194 008194 025195 007002 014204 014203 020010 ')

        sequence = text.strip().split(' ')
        summary = '9   4   1'
        map1 = ['1  35   0',
                '2   5   1   1',
                '3   4   0',
                '4   7   1   2',
                '5  12   0',
                '6   5   1   3',
                '7  16   0',
                '8   5   1   4',
                '9   7   0']
        reps = '-1   4  15   4'

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[23])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[25 + i])

        self.assertIn(reps, lines[35])

    def test_amv(self):

        ''' Nested, delayed replications, long sequence, 201 operators     '''

        sequence = ['310077']
        summary = '12   7   2'
        map1 = ['1  36   0',
                '2   4   1   1',
                '3  13   1   2',
                '4   8   1   3',
                '5   3   2   3   4',
                '6   1   1   3',
                '7   3   2   3   5',
                '8  15   0',
                '9   2   1   6',
                '10   9   0',
                '11  17   1   7',
                '12   1   0']
        reps = '-1  -1  -1  -1  -1   4  -1'

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[8])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[10 + i])

        self.assertIn(reps, lines[23])

    def test_atms(self):

        ''' Nested, delayed replications, 201, 202 and 207 operators       '''

        text = ('001007 008070 001033 001034 008070 001033 001034 002019 '
                '301011 301012 207003 004006 207000 304030 301021 007024 '
                '005021 007025 005022 008075 201133 005041 201000 005045 '
                '005043 005040 010001 201129 007002 201000 021166 008012 '
                '020010 020014 033078 055023 025214 008205 002224 025206 '
                '025206 025207 025206 025220 002019 033079 033080 108022 '
                '005042 025076 025077 025078 012163 012158 012159 033081 '
                '055007 002019 002165 033075 107003 008076 006029 006029 '
                '025140 025141 033076 033077 033003 055007 104000 031002 '
                '201133 005042 201000 014044 055007 040026 103000 031002 '
                '202124 025050 202000 101003 040016 014204 014203 002019 '
                '109000 031002 005042 008076 008023 014043 015042 008023 '
                '014043 015042 008023')

        sequence = text.strip().split(' ')
        summary = '11   6   1'
        map1 = ['1  47   0',
                '2   8   1   1',
                '3   4   0',
                '4   7   1   2',
                '5   2   0',
                '6   2   1   3',
                '7   2   0',
                '8   1   1   4',
                '9   1   1   5',
                '10   3   0',
                '11   9   1   6']
        reps = '22   3  -1  -1   3  -1'

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[20])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[22 + i])

        self.assertIn(reps, lines[34])

    def test_gpsro(self):

        ''' Nested, delayed replications, 2 levels                         '''

        sequence = ['310026']
        summary = '7   4   2'
        map1 = ['1  37   0',
                '2   3   1   1',
                '3   6   2   1   2',
                '4   1   1   1',
                '5   6   1   3',
                '6  10   1   4',
                '7   7   0']
        reps = '-1  -1  -1  -1'

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[8])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[10 + i])

        self.assertIn(reps, lines[18])

    def test_iasi(self):

        ''' Nested, delayed replications, 15 elements, 203 operators       '''

        text = ('001007 004001 004002 004003 004004 004005 004006 005001 '
                '006001 008012 008070 001031 008070 001031 005040 055023 '
                '002223 201130 005041 201000 005043 025214 008205 002224 '
                '025206 025207 002048 055024 055025 002048 055024 055026 '
                '202126 007001 202000 010001 007024 005021 007025 005022 '
                '025220 002151 012064 002151 012064 002151 012064 105020 '
                '002150 025076 025077 025078 012163 101010 014201 025194 '
                '008194 025195 002019 033060 033061 033062 033063 033064 '
                '033065 103010 025140 025141 025142 055007 104000 031002 '
                '201136 005042 201000 014046 055007 202124 101000 031002 '
                '025050 202000 203015 025052 203255 101003 025052 002019 '
                '025051 109007 005060 005061 025085 105006 005042 025142 '
                '014047 025142 014048 014204 014203')

        sequence = text.strip().split(' ')
        summary = '15   8   2'
        map1 = ['1  43   0',
                '2   5   1   1',
                '3   1   1   2',
                '4  10   0',
                '5   3   1   3',
                '6   1   0',
                '7   2   1   4',
                '8   1   0',
                '9   1   1   5',
                '10   0   0',
                '11   1   1   6',
                '12   2   0',
                '13   3   1   7',
                '14   5   2   7   8',
                '15   2   0']
        reps = '20  10  10  -1  -1   3   7   6'

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[20])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[22 + i])

        self.assertIn(reps, lines[38])

    def test_mwts(self):

        ''' Nested operators                                               '''

        text = ('001033 001034 001007 002019 005040 201136 005041 201000 '
                '005043 301011 301013 301021 007002 013040 007024 005021 '
                '007025 005022 304030 304031 025084 033082 002019 012064 '
                '033083 106000 031001 005042 201132 202129 012063 202000 '
                '201000 002019 012064 033083 106000 031001 005042 201132 '
                '202129 012063 202000 201000 002019 012064 033083 106000 '
                '031001 005042 201132 202129 012063 202000 201000')

        sequence = text.strip().split(' ')
        summary = '6   3   1'
        map1 = ['1  32   0',
                '2   2   1   1',
                '3   3   0',
                '4   2   1   2',
                '5   3   0',
                '6   2   1   3']
        reps = '-1  -1  -1'

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))
        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[14])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[16 + i])

        self.assertIn(reps, lines[23])

    def test_abicsr(self):

        ''' Bit maps, quality placeholders                                 '''

        text = ('301072 030021 030022 008012 007024 005021 007025 005022 '
                '010002 101010 304032 105002 002152 002024 007004 007004 '
                '013003 101010 304033 222000 236000 101161 031031 001031 '
                '001032 101030 033007 222000 237000 001031 001032 008033 '
                '101030 033007 222000 237000 001031 001032 008033 101030 '
                '033007 222000 237000 001031 001032 008033 101030 033007 '
                '222000 237000 001031 001032 008033 101030 033007 224000 '
                '237000 001031 001032 008023 101030 224255 224000 237000 '
                '001031 001032 008023 101030 224255')

        sequence = text.strip().split(' ')
        summary = '19  11   1'
        map1 = ['1  21   0',
                '2   5   1   1',
                '3   5   1   2',
                '4   8   1   3',
                '5   1   1   4',
                '6   2   0',
                '7   1   1   5',
                '8   3   0',
                '9   1   1   6',
                '10   3   0',
                '11   1   1   7',
                '12   3   0',
                '13   1   1   8',
                '14   3   0',
                '15   1   1   9',
                '16   3   0',
                '17   1   1  10',
                '18   3   0',
                '19   1   1  11']
        reps = '10   2  10 161  30  30  30  30  30  30  30'

        table = elements.process(sequence)
        with patch('sys.stdout', new=StringIO()) as out:
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))

        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[16])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[18 + i])

        self.assertIn(reps, lines[38])

    def test_tamdar(self):

        ''' Associated data with 204 operators                             '''

        text = ('301011 301013 301021 007002 007010 001008 204002 031021 '
                '001013 012001 011001 011002 002064 011037 011038 020041 '
                '204000 204007 031021 013009 204000 204001 031021 013009 '
                '013009 204000')

        sequence = text.strip().split(' ')
        summary = '1   0   0'
        map1 = ['1  44   0']

        with patch('sys.stdout', new=StringIO()) as out:
            table = elements.process(sequence)
            print(table)

        self.assertEqual(table.sequences[1][0].nseq, len(sequence))
        
        lines = out.getvalue().split('\n')

        self.assertIn(summary, lines[12])
        for i in range(len(map1)):
            self.assertIn(map1[i], lines[14 + i])

        self.assertIn('WARNING', lines[0])


if __name__ == '__main__':
    unittest.main()
